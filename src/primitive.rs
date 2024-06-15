//! Simple building-block data that can be read in one go.
//! All primitives have a known, fixed size.
//! Also, primitives advance the file pointer when read.

use crate::prelude::*;

pub(crate) trait SplitChecked: Sized {
    fn split_checked(&mut self, at: usize) -> Option<Self>;
}
impl<'a> SplitChecked for &'a [u8] {
    #[inline]
    fn split_checked(&mut self, at: usize) -> Option<&'a [u8]> {
        if at > self.len() {
            None
        } else {
            let (extracted, remainder) = self.split_at(at);
            *self = remainder;
            Some(extracted)
        }
    }
}

/// Implemented on integer types for reading as big-endian.
pub(crate) trait IntRead: Sized {
    /// Reads a big-endian integer.
    fn read(data: &mut &[u8]) -> StdResult<Self, &'static ErrorKind>;
}
/// Reads the int from u7 bytes, that is, the top bit in all bytes is ignored.
/// For raw reading on integer types, use `read_raw`.
pub(crate) trait IntReadBottom7: Sized {
    /// Read an int from bytes, but only using the bottom 7 bits of each byte.
    fn read_u7(data: &mut &[u8]) -> StdResult<Self, &'static ErrorKind>;
}

/// Implement simple big endian integer reads.
macro_rules! impl_read_int {
    {$( $int:ty ),*} => {
        $(
            impl IntRead for $int {
                #[inline]
                fn read(raw: &mut &[u8]) -> StdResult<$int, &'static ErrorKind> {
                    let bytes = raw.split_checked(mem::size_of::<$int>())
                        .ok_or(err_invalid!("failed to read the expected integer"))?;
                    Ok(bytes.iter().fold(0,|mut acc,byte| {
                        acc=acc.checked_shl(8).unwrap_or(0);
                        acc|=*byte as $int;
                        acc
                    }))
                }
            }
        )*
    }
}
impl_read_int! {u8,u16,u32}

/// Slightly restricted integers.
macro_rules! int_feature {
    { $name:ident ; $inner:tt : read_u7 } => {
        impl IntReadBottom7 for $name {
            fn read_u7(raw: &mut &[u8]) -> StdResult<$name, &'static ErrorKind> {
                let bytes = raw.split_checked(mem::size_of::<$inner>())
                    .ok_or(err_invalid!("failed to read the expected integer"))?;
                if cfg!(feature = "strict") {
                    ensure!(bytes.iter().all(|byte| bit_range!(*byte, 7..8)==0), err_malformed!("invalid byte with top bit set"));
                }
                let raw = bytes.iter().fold(0, |mut acc,byte| {
                    acc <<= 7;
                    acc |= bit_range!(*byte, 0..7) as $inner;
                    acc
                });
                Ok(if cfg!(feature = "strict") {
                    Self::try_from(raw).ok_or(err_malformed!(stringify!("expected " $name ", found " $inner)))?
                }else{
                    //Ignore and truncate extra bits
                    Self::from(raw)
                })
            }
        }
    };
    { $name:ident ; $inner:tt : read } => {
        impl IntRead for $name {
            #[inline]
            fn read(raw: &mut &[u8]) -> StdResult<Self, &'static ErrorKind> {
                let raw = $inner::read(raw)?;
                if cfg!(feature = "strict") {
                    Ok(Self::try_from(raw).ok_or(err_malformed!(concat!("expected ", stringify!($name), ", found ", stringify!($inner))))?)
                }else{
                    //Throw away extra bits
                    Ok(Self::from(raw))
                }
            }
        }
    };
}
macro_rules! restricted_int {
    {$(#[$attr:meta])* $name:ident : $inner:tt => $bits:expr ; $( $feature:tt )* } => {
        $(#[$attr])*
        #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Default)]
        #[repr(transparent)]
        #[allow(non_camel_case_types)]
        pub struct $name($inner);
        impl From<$inner> for $name {
            /// Lossy conversion, loses top bit.
            #[inline]
            fn from(raw: $inner) -> $name {
                $name::from_int_lossy(raw)
            }
        }
        impl From<$name> for $inner {
            #[inline]
            fn from(restricted: $name) -> $inner {restricted.0}
        }
        impl fmt::Display for $name {
            #[inline]
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                fmt::Display::fmt(&self.0, f)
            }
        }
        impl $name {
            const MASK: $inner = (1 << $bits) - 1;

            /// The maximum value that this restricted integer can hold.
            #[inline]
            pub const fn max_value() -> $name {
                $name (Self::MASK)
            }

            /// Creates a restricted int from its non-restricted counterpart by masking off the
            /// extra bits.
            #[inline]
            pub const fn new(raw: $inner) -> $name {
                $name (raw & Self::MASK)
            }

            /// Creates a restricted int from its non-restricted counterpart by masking off the
            /// extra bits.
            #[inline]
            pub const fn from_int_lossy(raw: $inner) -> $name {
                $name (raw & Self::MASK)
            }

            /// Returns `Some` if the raw integer is within range of the restricted integer, and
            /// `None` otherwise.
            #[inline]
            pub fn try_from(raw: $inner) -> Option<$name> {
                if raw <= Self::MASK {
                    Some($name(raw))
                }else{
                    None
                }
            }

            /// Get the inner integer out of the wrapper.
            /// The inner integer is guaranteed to be in range of the restricted wrapper.
            #[inline]
            pub fn as_int(self) -> $inner {
                Into::into(self)
            }

            /// Cast a slice of raw integers to a slice of restricted integers, only if there are
            /// no out-of-range integers.
            #[inline]
            pub fn slice_try_from_int(raw: &[$inner]) -> Option<&[$name]> {
                for &int in raw {
                    if int > Self::MASK {
                        return None;
                    }
                }
                unsafe {
                    Some(Self::slice_from_int_unchecked(raw))
                }
            }

            /// Cast a slice of raw integers to a slice of restricted integers.
            ///
            /// The slice is truncated up to the first out-of-range integer, if there is any.
            #[inline]
            pub fn slice_from_int(raw: &[$inner]) -> &[$name] {
                let first_oob = raw
                    .iter()
                    .position(|&b| b > Self::MASK)
                    .unwrap_or(raw.len());
                unsafe {
                    Self::slice_from_int_unchecked(&raw[..first_oob])
                }
            }

            /// Cast a slice of raw integers to a slice of restricted integers.
            ///
            /// # Safety
            ///
            /// The input slice must not contain any out-of-range integers.
            #[inline]
            pub unsafe fn slice_from_int_unchecked(raw: &[$inner]) -> &[$name] {
                &*( raw as *const [$inner] as *const [$name] )
            }

            /// Cast a slice of mutable raw integers to a slice of mutable restricted integers, only
            /// if there are no out-of-range integers.
            #[inline]
            pub fn slice_try_from_int_mut(raw: &mut [$inner]) -> Option<&mut [$name]> {
                for &int in raw.iter() {
                    if int > Self::MASK {
                        return None;
                    }
                }
                unsafe {
                    Some(Self::slice_from_int_unchecked_mut(raw))
                }
            }

            /// Cast a slice of mutable raw integers to a slice of mutable restricted integers.
            ///
            /// The slice is truncated up to the first out-of-range integer, if there is any.
            #[inline]
            pub fn slice_from_int_mut(raw: &mut [$inner]) -> &mut [$name] {
                let first_oob = raw
                    .iter()
                    .position(|&b| b > Self::MASK)
                    .unwrap_or(raw.len());
                unsafe {
                    Self::slice_from_int_unchecked_mut(&mut raw[..first_oob])
                }
            }

            /// Cast a slice of mutable raw integers to a slice of mutable restricted integers.
            ///
            /// # Safety
            ///
            /// The input slice must not contain any out-of-range integers.
            #[inline]
            pub unsafe fn slice_from_int_unchecked_mut(raw: &mut [$inner]) -> &mut [$name] {
                &mut *( raw as *mut [$inner] as *mut [$name] )
            }

            /// Cast a slice of restricted integers to the corresponding raw integers.
            ///
            /// All integers are guaranteed to be within range of the restricted int.
            #[inline]
            pub fn slice_as_int(slice: &[$name]) -> &[$inner] {
                unsafe { &*(slice as *const [$name] as *const [$inner]) }
            }

            #[allow(dead_code)]
            #[inline]
            pub(crate) fn check_int(raw: $inner) -> StdResult<$name, &'static ErrorKind> {
                Self::try_from(raw).ok_or_else(
                    || err_invalid!("invalid integer with top bits set")
                )
            }
        }
        impl PartialEq<$inner> for $name {
            fn eq(&self, rhs: &$inner) -> bool {
                self.as_int() == *rhs
            }
        }
        impl PartialOrd<$inner> for $name {
            fn partial_cmp(&self, rhs: &$inner) -> Option<core::cmp::Ordering> {
                Some(self.as_int().cmp(rhs))
            }
        }
        impl PartialEq<$name> for $inner {
            fn eq(&self, rhs: &$name) -> bool {
                *self == rhs.as_int()
            }
        }
        impl PartialOrd<$name> for $inner {
            fn partial_cmp(&self, rhs: &$name) -> Option<core::cmp::Ordering> {
                Some(self.cmp(&rhs.as_int()))
            }
        }
        impl core::ops::Add for $name {
            type Output = Self;
            fn add(self, other: Self) -> Self {
                Self::new(self.as_int() + other.as_int())
            }
        }
        impl core::ops::Sub for $name {
            type Output = Self;
            fn sub(self, other: Self) -> Self {
                Self::new(self.as_int() - other.as_int())
            }
        }
        impl core::ops::AddAssign for $name {
            fn add_assign(&mut self, other: Self) {
                *self = *self + other
            }
        }
        impl core::ops::SubAssign for $name {
            fn sub_assign(&mut self, other: Self) {
                *self = *self - other
            }
        }
        impl core::ops::BitOr for $name {
            type Output = Self;
            fn bitor(self, other: Self) -> Self {
                Self::new(self.as_int() | other.as_int())
            }
        }
        impl core::ops::BitAnd for $name {
            type Output = Self;
            fn bitand(self, other: Self) -> Self {
                Self::new(self.as_int() & other.as_int())
            }
        }
        impl core::ops::BitXor for $name {
            type Output = Self;
            fn bitxor(self, other: Self) -> Self {
                Self::new(self.as_int() ^ other.as_int())
            }
        }
        impl core::ops::BitOrAssign for $name {
            fn bitor_assign(&mut self, other: Self) {
                *self = *self | other
            }
        }
        impl core::ops::BitAndAssign for $name {
            fn bitand_assign(&mut self, other: Self) {
                *self = *self & other
            }
        }
        impl core::ops::BitXorAssign for $name {
            fn bitxor_assign(&mut self, other: Self) {
                *self = *self ^ other
            }
        }

        $( int_feature!{$name ; $inner : $feature} )*
    };
}
restricted_int! {
    /// A 15-bit integer type.
    ///
    /// Wraps the `u16` type and ensures that the top bit is always zero.
    u15: u16 => 15; read
}
restricted_int! {
    /// A 14-bit integer type.
    ///
    /// Wraps the `u16` type and ensures that the top two bits are always zero.
    u14: u16 => 14; read read_u7
}
restricted_int! {
    /// A 7-bit integer type.
    ///
    /// Wraps the `u8` type and ensures that the top bit is always zero.
    u7: u8 => 7; read
}
restricted_int! {
    /// A 4-bit integer type.
    ///
    /// Wraps the `u8` type and ensures that the top 4 bits are always zero.
    u4: u8 => 4; read
}
restricted_int! {
    /// A 2-bit integer type.
    ///
    /// Wraps the `u8` type and ensures that the top 6 bits are always zero.
    u2: u8 => 2; read
}
restricted_int! {
    /// A 24-bit integer type.
    ///
    /// Wraps the `u32` type and ensures that the top 8 bits are always zero.
    u24: u32 => 24;
}
impl IntRead for u24 {
    fn read(raw: &mut &[u8]) -> StdResult<u24, &'static ErrorKind> {
        let bytes = raw
            .split_checked(3)
            .ok_or(err_invalid!("failed to read u24 bytes"))?;
        //Using lossy `from` because value is guaranteed to be 24 bits (3 bytes)
        Ok(u24::from(bytes.iter().fold(0, |mut acc, byte| {
            acc <<= 8;
            acc |= *byte as u32;
            acc
        })))
    }
}

restricted_int! {
    /// Referred to in the MIDI spec as "variable length int".
    u28: u32 => 28;
}
impl IntReadBottom7 for u28 {
    fn read_u7(raw: &mut &[u8]) -> StdResult<u28, &'static ErrorKind> {
        let mut int: u32 = 0;
        for _ in 0..4 {
            let byte = match raw.split_checked(1) {
                Some(slice) => slice[0],
                None => {
                    if cfg!(feature = "strict") {
                        bail!(err_malformed!("unexpected eof while reading varlen int"))
                    } else {
                        //Stay with what was read
                        break;
                    }
                }
            };
            int <<= 7;
            int |= bit_range!(byte, 0..7) as u32;
            if bit_range!(byte, 7..8) == 0 {
                //Since we did at max 4 reads of 7 bits each, there MUST be at max 28 bits in this int
                //Therefore it's safe to call lossy `from`
                return Ok(u28::from(int));
            }
        }
        if cfg!(feature = "strict") {
            Err(err_malformed!("varlen integer larger than 4 bytes"))
        } else {
            //Use the 4 bytes as-is
            Ok(u28::from(int))
        }
    }
}

impl u28 {
    pub(crate) fn write_varlen<W: Write>(&self, out: &mut W) -> WriteResult<W> {
        let int = self.as_int();
        let mut skipping = true;
        for i in (0..4).rev() {
            let byte = ((int >> (i * 7)) & 0x7F) as u8;
            if skipping && byte == 0 && i != 0 {
                //Skip these leading zeros
            } else {
                //Write down this u7
                skipping = false;
                let byte = if i == 0 {
                    //Last byte
                    byte
                } else {
                    //Leading byte
                    byte | 0x80
                };
                out.write(&[byte])?;
            }
        }
        Ok(())
    }
}

/// Reads a slice represented in the input as a `u28` `len` followed by `len` bytes.
pub(crate) fn read_varlen_slice<'a>(raw: &mut &'a [u8]) -> Result<&'a [u8]> {
    let len = u28::read_u7(raw)
        .context(err_invalid!("failed to read varlen slice length"))?
        .as_int();
    Ok(match raw.split_checked(len as usize) {
        Some(slice) => slice,
        None => {
            if cfg!(feature = "strict") {
                bail!(err_malformed!("incomplete varlen slice"))
            } else {
                mem::replace(raw, &[])
            }
        }
    })
}

/// Write a slice represented as a varlen `u28` as its length and then the raw bytes.
pub(crate) fn write_varlen_slice<W: Write>(slice: &[u8], out: &mut W) -> WriteResult<W> {
    let len = u32::try_from(slice.len())
        .ok()
        .and_then(u28::try_from)
        .ok_or_else(|| W::invalid_input("varlen slice exceeds 28 bits"))?;
    len.write_varlen(out)?;
    out.write(slice)?;
    Ok(())
}

/// The order in which tracks should be laid out when playing back this SMF file.
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Format {
    /// This file should have a single track only.
    ///
    /// If the `strict` feature is enabled, an error is raised if the format is
    /// `Format::SingleTrack` and there is not exactly one track.
    SingleTrack,
    /// This file has several tracks that should be played simultaneously.
    ///
    /// Usually the first track controls tempo and other song metadata.
    Parallel,
    /// This file has several tracks, each one a separate song.
    ///
    /// The tracks should be played sequentially, as completely separate MIDI tracks packaged
    /// within a single SMF file.
    Sequential,
}
impl Format {
    pub(crate) fn read(raw: &mut &[u8]) -> Result<Format> {
        let format = u16::read(raw)?;
        Ok(match format {
            0 => Format::SingleTrack,
            1 => Format::Parallel,
            2 => Format::Sequential,
            _ => bail!(err_invalid!("invalid smf format")),
        })
    }

    pub(crate) fn encode(&self) -> [u8; 2] {
        let code: u16 = match self {
            Format::SingleTrack => 0,
            Format::Parallel => 1,
            Format::Sequential => 2,
        };
        code.to_be_bytes()
    }
}

/// The timing for an SMF file.
/// This can be in ticks/beat or ticks/second.
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Timing {
    /// Specifies ticks/beat as a 15-bit integer.
    ///
    /// The length of a beat is not standard, so in order to fully describe the length of a MIDI
    /// tick the [`MetaMessage::Tempo`](enum.MetaMessage.html#Tempo.v) event should be present.
    Metrical(u15),
    /// Specifies ticks/second by dividing a second into frames and then into subframes.
    /// Therefore the length of of a tick is `1/fps/subframe`.
    Timecode(Fps, u8),
}
impl Timing {
    pub(crate) fn read(raw: &mut &[u8]) -> Result<Timing> {
        let raw =
            u16::read(raw).context(err_invalid!("unexpected eof when reading midi timing"))?;
        if bit_range!(raw, 15..16) != 0 {
            //Timecode
            let fps = -(bit_range!(raw, 8..16) as i8);
            let subframe = bit_range!(raw, 0..8) as u8;
            Ok(Timing::Timecode(
                Fps::from_int(fps as u8).ok_or(err_invalid!("invalid smpte fps"))?,
                subframe,
            ))
        } else {
            //Metrical
            Ok(Timing::Metrical(u15::from(raw)))
        }
    }

    pub(crate) fn encode(&self) -> [u8; 2] {
        match self {
            Timing::Metrical(ticksperbeat) => ticksperbeat.as_int().to_be_bytes(),
            Timing::Timecode(framespersec, ticksperframe) => {
                [(-(framespersec.as_int() as i8)) as u8, *ticksperframe]
            }
        }
    }
}

/// A timestamp encoding an SMPTE time of the day.
///
/// Enforces several guarantees:
///
/// - `hour` is inside [0, 23]
/// - `minute` is inside [0, 59]
/// - `second` is inside [0, 59]
/// - `frame` is inside [0, fps - 1]
/// - `subframe` is inside [0, 99]
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct SmpteTime {
    hour: u8,
    minute: u8,
    second: u8,
    frame: u8,
    subframe: u8,
    fps: Fps,
}
impl SmpteTime {
    /// Create a new SMPTE timestamp with the given information.
    #[inline]
    pub fn new(
        hour: u8,
        minute: u8,
        second: u8,
        frame: u8,
        subframe: u8,
        fps: Fps,
    ) -> Option<SmpteTime> {
        macro_rules! check {
            ($cond:expr) => {{
                if !{ $cond } {
                    return None;
                }
            }};
        }
        check!(hour < 24);
        check!(minute < 60);
        check!(second < 60);
        check!(frame < fps.as_int());
        check!(subframe < 100);
        Some(SmpteTime {
            hour,
            minute,
            second,
            frame,
            subframe,
            fps,
        })
    }

    /// Get the hour component of this timestamp.
    #[inline]
    pub fn hour(&self) -> u8 {
        self.hour
    }

    /// Get the minute component of this timestamp.
    #[inline]
    pub fn minute(&self) -> u8 {
        self.minute
    }

    /// Get the second component of this timestamp.
    #[inline]
    pub fn second(&self) -> u8 {
        self.second
    }

    /// Get the frame component of this timestamp.
    /// The meaning of this value depends on the value of `fps`.
    #[inline]
    pub fn frame(&self) -> u8 {
        self.frame
    }

    /// Get the subframe component of this timestamp (hundredths of a frame).
    #[inline]
    pub fn subframe(&self) -> u8 {
        self.subframe
    }

    /// Get the FPS component of this timestamp.
    #[inline]
    pub fn fps(&self) -> Fps {
        self.fps
    }

    /// Convert the second + frame + subframe components of this timestamp into a single
    /// floating-point number of seconds.
    /// Note that this does not include the hour and minute components.
    #[inline]
    pub fn second_f32(&self) -> f32 {
        self.second as f32
            + ((self.frame as f32 + self.subframe as f32 / 100.0) / self.fps.as_f32())
    }

    pub(crate) fn read(raw: &mut &[u8]) -> Result<SmpteTime> {
        let data = raw
            .split_checked(5)
            .ok_or(err_invalid!("failed to read smpte time data"))?;
        let hour_fps = data[0];
        let (hour, fps) = (bit_range!(hour_fps, 0..5), bit_range!(hour_fps, 5..7));
        let fps = Fps::from_code(u2::from(fps));
        let minute = data[1];
        let second = data[2];
        let frame = data[3];
        let subframe = data[4];
        Ok(SmpteTime::new(hour, minute, second, frame, subframe, fps)
            .ok_or(err_invalid!("invalid smpte time"))?)
    }

    pub(crate) fn encode(&self) -> [u8; 5] {
        let hour_fps = self.hour() | self.fps().as_code().as_int() << 5;
        [
            hour_fps,
            self.minute(),
            self.second(),
            self.frame(),
            self.subframe(),
        ]
    }
}

/// One of the four FPS values available for SMPTE times, as defined by the MIDI standard.
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Fps {
    /// 24 frames per second.
    Fps24,
    /// 25 frames per second.
    Fps25,
    /// Actually `29.97 = 30 / 1.001` frames per second.
    ///
    /// Quite an exotic value because of interesting historical reasons.
    Fps29,
    /// 30 frames per second.
    Fps30,
}
impl Fps {
    /// Does the conversion from a 2-bit fps code to an `Fps` value.
    pub(crate) fn from_code(code: u2) -> Fps {
        match code.as_int() {
            0 => Fps::Fps24,
            1 => Fps::Fps25,
            2 => Fps::Fps29,
            3 => Fps::Fps30,
            _ => unreachable!(),
        }
    }

    /// Does the conversion to a 2-bit fps code.
    pub(crate) fn as_code(self) -> u2 {
        u2::from(match self {
            Fps::Fps24 => 0,
            Fps::Fps25 => 1,
            Fps::Fps29 => 2,
            Fps::Fps30 => 3,
        })
    }

    /// Converts an integer representing the semantic fps to an `Fps` value (ie. `24` -> `Fps24`).
    #[inline]
    pub fn from_int(raw: u8) -> Option<Fps> {
        Some(match raw {
            24 => Fps::Fps24,
            25 => Fps::Fps25,
            29 => Fps::Fps29,
            30 => Fps::Fps30,
            _ => return None,
        })
    }

    /// Get the integral approximate fps out.
    #[inline]
    pub fn as_int(self) -> u8 {
        match self {
            Fps::Fps24 => 24,
            Fps::Fps25 => 25,
            Fps::Fps29 => 29,
            Fps::Fps30 => 30,
        }
    }

    /// Get the actual `f32` fps out.
    #[inline]
    pub fn as_f32(self) -> f32 {
        match self.as_int() {
            24 => 24.0,
            25 => 25.0,
            29 => 30.0 / 1.001,
            30 => 30.0,
            _ => unreachable!(),
        }
    }
}
impl From<Fps> for f32 {
    fn from(x: Fps) -> Self {
        x.as_f32()
    }
}
impl From<Fps> for u8 {
    fn from(x: Fps) -> Self {
        x.as_int()
    }
}
