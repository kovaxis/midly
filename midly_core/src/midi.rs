//! Midi message definitions.

use crate::prelude::*;

/// A general MIDI message.
///
/// # About MIDI
///
/// MIDI can be transmitted through many different channels: 5-pin MIDI cables, USB, Ethernet,
/// the Internet, `.mid` files, etc, and the types of MIDI messages carried in each channel differ.
/// For example, when receiving messages from a "live" connection, such as a MIDI keyboard connected
/// to your machine, the keyboard will send mostly `Channel` messages, maybe some `ActiveSensing`
/// messages to indicate that it is still connected, but it will never send, for example, a
/// `TrackName` message.
///
/// On the other hand, when reading a `.mid` file, it may very well contain a `TrackName` message,
/// maybe some `Copyright` messages, `Lyric`s and other kind of metadata.
///
/// This kind of separation is acknowledged by the MIDI standard, and in fact MIDI messages are
/// divided into three kinds:
///
/// - Channel messages: `NoteOn`, `NoteOff`, `PitchBend`, etc...
/// - System messages: `SysEx`, `SongPosition`, `ActiveSensing`, etc...
///     This category is sometimes subdivided into two sub-categories:
///     - System common messages: `SysEx`, `SongPosition`, etc...
///     - System realtime messages: `ActiveSensing`, `Reset`, etc...
/// - Meta messages: `TrackName`, `Copyright`, `Lyrics`, etc, inherent to `.mid` files.
///
/// Because `MidiMessage` is a general MIDI type, all of these are supported, but when reading from
/// a live channel you can expect no _meta-messages_ to occur.
/// Similarly, static `.mid` files have no native support for most system messages, but there are
/// methods to freeze all kinds of MIDI messages in a `.mid` file, so this cannot be relied on.
///
/// # Categorization
///
/// Note that despite MIDI messages being clearly categorized and separated into 3-4 different
/// classes, there is no `SystemMessage` or `MetaMessage` type, despite there being a
/// `ChannelMessage` type.
///
/// This is for performance and memory usage reasons: nesting types forces Rust to add extra padding
/// to satisfy alignment restrictions, and so the `MidiMessage` type is as flat as possible.
///
/// However, `ChannelMessage` is left as an exception: it is the most common MIDI message type and
/// due to its structure, nesting incurs no extra cost.
///
/// To classify messages into channel, system and meta messages, use the [`classify()`] method or
/// the various `is_*()` methods.
///
/// The benefit of this strange layout is that MIDI messages are only 24 bytes in 64-bit platforms,
/// and that matching on messages is less verbose and requires less imports.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MidiMessage<'a> {
    /// A message associated to a channel, carrying note playback data.
    /// This is the most common MIDI message type.
    Channel {
        /// The channel that this message is associated to.
        /// This is usually in the range `[0, 15]`.
        channel: u8,
        /// The type of message sent and its associated data.
        msg: ChannelMessage,
    },

    /// A **sys**tem-**ex**clusive message, carrying arbitrary data bytes.
    ///
    /// The inner data usually starts with an `0xF0` byte and ends with a `0xF7` byte, though this
    /// is not always guaranteed.
    ///
    /// In particular, since some sequencers require timed sysex packets, `.mid` files are allowed
    /// to split a single sysex packet into an initial incomplete `SysEx` message (without a
    /// finalizing `0xF7` byte) and further `Unspecified` messages, without an initial `0xF0` byte
    /// but with a final `0xF7` byte, completing the incomplete `SysEx` message.
    ///
    /// This is a System Common message.
    SysEx(Bytes<'a>),
    /// A single nibble of a MIDI Time Code Quarter Frame Message, carrying a tag type and a 4 bits
    /// of a tag value.
    ///
    /// This is a System Common message.
    Mtc(MtcKind, u8),
    /// The number of MIDI beats (6 x MIDI clocks) that have elapsed since the start of the
    /// sequence.
    ///
    /// This is a System Common message.
    SongPosition(u16),
    /// Select a given song index.
    ///
    /// This is a System Common message.
    SongSelect(u8),
    /// Used with analog synthesizers to request that all oscillators be tuned.
    ///
    /// This is a System Common message.
    TuneRequest,
    /// Used to synchronize MIDI devices.
    /// If sent, these should be sent 24 times per quarter note.
    ///
    /// This is a System Realtime message.
    TimingClock,
    /// Indicates MIDI devices to start playing at the beggining of the sequence.
    ///
    /// This is a System Realtime message.
    Start,
    /// Indicates MIDI devices to continue playing from the current song position counter.
    ///
    /// This is a System Realtime message.
    Continue,
    /// Indicates MIDI devices to stop playing immediately.
    ///
    /// This is a System Realtime message.
    Stop,
    /// Used to make sure that a connection is still alive.
    /// Once one of these messages is transmitted, a message should arrive every 300ms or else the
    /// connection is considered broken.
    ///
    /// This is a System Realtime message.
    ActiveSensing,
    /// Indicates MIDI devices to reset to the power-up condition.
    ///
    /// This is a System Realtime message.
    Reset,
    /// An unspecified sequence of (presumably MIDI) bytes.
    ///
    /// Usually, this type is used to represent a sequence of bytes that could not be recognized as
    /// a known MIDI message.
    Unspecified(Bytes<'a>),

    /// Identifies the current track with a number.
    ///
    /// For sequential MIDI file types, `TrackNumber` can be empty, and defaults to
    /// the track index.
    ///
    /// This is a Meta-Message, only present in `.mid` files.
    TrackNumber(Option<u16>),
    /// Arbitrary text associated to a position in the file.
    ///
    /// This is a Meta-Message, only present in `.mid` files.
    Text(Bytes<'a>),
    /// Arbitrary copyright text.
    ///
    /// This is a Meta-Message, only present in `.mid` files.
    Copyright(Bytes<'a>),
    /// Identifies the current track with a human-readable name.
    ///
    /// This is a Meta-Message, only present in `.mid` files.
    TrackName(Bytes<'a>),
    /// Identifies the current track with a human-readable instrument name.
    ///
    /// This is a Meta-Message, only present in `.mid` files.
    InstrumentName(Bytes<'a>),
    /// Gives the lyrics at a certain position in the file.
    ///
    /// This is a Meta-Message, only present in `.mid` files.
    Lyric(Bytes<'a>),
    /// An arbitrary mark associated to a position in the file.
    ///
    /// This is a Meta-Message, only present in `.mid` files.
    Marker(Bytes<'a>),
    /// An arbitrary cue point associated to a position in the file.
    ///
    /// This is a Meta-Message, only present in `.mid` files.
    CuePoint(Bytes<'a>),
    /// Identifies the current track with a human-readable program name.
    ///
    /// This is a Meta-Message, only present in `.mid` files.
    ProgramName(Bytes<'a>),
    /// Indicates that this track is intended for the device with the given name.
    ///
    /// This is a Meta-Message, only present in `.mid` files.
    DeviceName(Bytes<'a>),
    /// Indicates that this track is intended for the given MIDI channel.
    ///
    /// This is a Meta-Message, only present in `.mid` files.
    MidiChannel(u8),
    /// Indicates that this track is intended for the given MIDI port.
    ///
    /// This is a Meta-Message, only present in `.mid` files.
    MidiPort(u8),
    /// Must appear as the last event of every track.
    ///
    /// This is a Meta-Message, only present in `.mid` files.
    EndOfTrack,
    /// The duration of a beat (quarter note), in microseconds.
    ///
    /// Usually appears at the beggining of a track, before any midi events are sent, but there
    /// are no guarantees.
    /// The usage of this event along with the metrical timing of a file is the most common way to
    /// specify the tempo of a track.
    ///
    /// This is a Meta-Message, only present in `.mid` files.
    Tempo(u32),
    /// Indicates an SMPTE time, as declared in the MIDI spec.
    ///
    /// This is a Meta-Message, only present in `.mid` files.
    SmpteOffset(SmpteTime),
    /// Indicates the time signature of this track.
    ///
    /// In order, the bytes represent: numerator, denominator, MIDI clocks per click and 32nd notes
    /// per quarter.
    ///
    /// This is a Meta-Message, only present in `.mid` files.
    TimeSignature(u8, u8, u8, u8),
    /// Information about the key signature of the track.
    ///
    /// The first byte indicates the number of flats/sharps of the scale.
    /// Negative numbers indicate the amount of flats, while positive numbers the amount of sharps.
    /// The second boolean indicates whether the scale is major (`false`) or minor (`true`).
    ///
    /// This is a Meta-Message, only present in `.mid` files.
    KeySignature(i8, bool),
    /// An arbitrary string of bytes indicated for a specific sequencer, with the first few bytes
    /// referring to the sequencer ID.
    /// Refer to the MIDI specification for more information.
    ///
    /// This is a Meta-Message, only present in `.mid` files.
    SequencerSpecific(Bytes<'a>),
    /// An unknown meta-message, with an associated type byte and data bytes.
    ///
    /// This event only appears on `.mid` files.
    UnknownMeta { kind: u8, data: Bytes<'a> },
}
impl<'a> MidiMessage<'a> {
    /// Removes any associated lifetimes by cloning referenced data.
    ///
    /// This method is only available with the `alloc` feature enabled.
    /// See the [`into_static()`] method for a non-`alloc` alternative.
    #[cfg(feature = "alloc")]
    #[inline]
    pub fn into_owned(self) -> MidiMessage<'static> {
        use self::MidiMessage::*;
        match self {
            Channel { channel, msg } => MidiMessage::Channel { channel, msg },

            SysEx(data) => SysEx(data.into_owned()),
            Mtc(v0, v1) => Mtc(v0, v1),
            SongPosition(v) => SongPosition(v),
            SongSelect(v) => SongSelect(v),
            TuneRequest => TuneRequest,
            TimingClock => TimingClock,
            Start => Start,
            Continue => Continue,
            Stop => Stop,
            ActiveSensing => ActiveSensing,
            Reset => Reset,
            Unspecified(data) => Unspecified(data.into_owned()),

            TrackNumber(v) => TrackNumber(v),
            Text(data) => Text(data.into_owned()),
            Copyright(data) => Copyright(data.into_owned()),
            TrackName(data) => TrackName(data.into_owned()),
            InstrumentName(data) => InstrumentName(data.into_owned()),
            Lyric(data) => Lyric(data.into_owned()),
            Marker(data) => Marker(data.into_owned()),
            CuePoint(data) => CuePoint(data.into_owned()),
            ProgramName(data) => ProgramName(data.into_owned()),
            DeviceName(data) => DeviceName(data.into_owned()),
            MidiChannel(v) => MidiChannel(v),
            MidiPort(v) => MidiPort(v),
            EndOfTrack => EndOfTrack,
            Tempo(v) => Tempo(v),
            SmpteOffset(time) => SmpteOffset(time),
            TimeSignature(v0, v1, v2, v3) => TimeSignature(v0, v1, v2, v3),
            KeySignature(v0, v1) => KeySignature(v0, v1),
            SequencerSpecific(data) => SequencerSpecific(data.into_owned()),
            UnknownMeta { kind, data } => UnknownMeta {
                kind,
                data: data.into_owned(),
            },
        }
    }

    /// Makes all referenced data owned instead, potentially cloning borrowed data.
    ///
    /// This method is only available with the `alloc` feature enabled.
    #[cfg(feature = "alloc")]
    #[inline]
    pub fn make_owned(&mut self) {
        *self = mem::replace(self, MidiMessage::Reset).into_owned();
    }

    /// Removes any associated lifetimes by removing borrowed data.
    /// This function will replace all bytestring data for empty bytestrings.
    #[inline]
    pub fn to_static(&self) -> MidiMessage<'static> {
        use self::MidiMessage::*;
        match *self {
            Channel { channel, msg } => MidiMessage::Channel { channel, msg },

            SysEx(_) => SysEx(Bytes::new()),
            Mtc(v0, v1) => Mtc(v0, v1),
            SongPosition(v) => SongPosition(v),
            SongSelect(v) => SongSelect(v),
            TuneRequest => TuneRequest,
            TimingClock => TimingClock,
            Start => Start,
            Continue => Continue,
            Stop => Stop,
            ActiveSensing => ActiveSensing,
            Reset => Reset,
            Unspecified(_) => Unspecified(Bytes::new()),

            TrackNumber(v) => TrackNumber(v),
            Text(_) => Text(Bytes::new()),
            Copyright(_) => Copyright(Bytes::new()),
            TrackName(_) => TrackName(Bytes::new()),
            InstrumentName(_) => InstrumentName(Bytes::new()),
            Lyric(_) => Lyric(Bytes::new()),
            Marker(_) => Marker(Bytes::new()),
            CuePoint(_) => CuePoint(Bytes::new()),
            ProgramName(_) => ProgramName(Bytes::new()),
            DeviceName(_) => DeviceName(Bytes::new()),
            MidiChannel(v) => MidiChannel(v),
            MidiPort(v) => MidiPort(v),
            EndOfTrack => EndOfTrack,
            Tempo(v) => Tempo(v),
            SmpteOffset(time) => SmpteOffset(time),
            TimeSignature(v0, v1, v2, v3) => TimeSignature(v0, v1, v2, v3),
            KeySignature(v0, v1) => KeySignature(v0, v1),
            SequencerSpecific(_) => SequencerSpecific(Bytes::new()),
            UnknownMeta { kind, data: _ } => UnknownMeta {
                kind,
                data: Bytes::new(),
            },
        }
    }

    /// Clears any bytestrings, removing any references to external data.
    #[inline]
    pub fn make_static(&mut self) {
        *self = self.to_static();
    }

    /// Attempt to decode a sequence of MIDI bytes into a `MidiMessage`.
    /// If the message is malformed or unrecognized, `None` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// # use midly_core::MidiMessage;
    /// let bytes = &[0x2A];
    /// let msg = MidiMessage::decode(bytes);
    ///
    /// assert_eq!(msg, None);
    /// ```
    #[inline]
    pub fn try_decode(data: &'a [u8]) -> Option<MidiMessage<'a>> {
        let status = *data.get(0)?;
        let channel = status & 0xF;
        macro_rules! chan {
            ($evname:ident, $($fname:ident: $num:literal),*) => {
                MidiMessage::Channel{
                    channel,
                    msg: ChannelMessage::$evname {$(
                        $fname: *data.get($num)?,
                    )*},
                }
            }
        }
        Some(match status >> 4 {
            0x8 => chan!(NoteOff, key: 1, vel: 2),
            0x9 => chan!(NoteOn, key: 1, vel: 2),
            0xA => chan!(Aftertouch, key: 1, vel: 2),
            0xB => chan!(Controller, controller: 1, value: 2),
            0xC => chan!(ProgramChange, program: 1),
            0xD => chan!(ChannelAftertouch, vel: 1),
            0xE => MidiMessage::Channel {
                channel,
                msg: ChannelMessage::PitchBend {
                    bend: PitchBend::from_u16(
                        (*data.get(1)? as u16) | ((*data.get(2)? as u16) << 7),
                    ),
                },
            },
            0xF => match channel {
                0x0 => MidiMessage::SysEx(data.into()),
                0x1 => {
                    MidiMessage::Mtc(MtcKind::from_bits(*data.get(1)? >> 4), *data.get(1)? & 0xF)
                }
                0x2 => MidiMessage::SongPosition(
                    (*data.get(1)? as u16) | ((*data.get(2)? << 7) as u16),
                ),
                0x3 => MidiMessage::SongSelect(*data.get(1)?),
                0x6 => MidiMessage::TuneRequest,
                0x8 => MidiMessage::TimingClock,
                0xA => MidiMessage::Start,
                0xB => MidiMessage::Continue,
                0xC => MidiMessage::Stop,
                0xE => MidiMessage::ActiveSensing,
                0xF => MidiMessage::Reset,
                _ => return None,
            },
            _ => return None,
        })
    }

    /// Attempt to decode a sequence of MIDI bytes into a `MidiMessage`.
    /// If the message is malformed or unrecognized, `Unspecified` is returned with the raw data
    /// bytes.
    ///
    /// # Examples
    ///
    /// ```
    /// # use midly_core::{MidiMessage, ChannelMessage};
    /// let bytes = &[0x90, 0x40, 0x20];
    /// let msg = MidiMessage::decode(bytes);
    ///
    /// assert_eq!(msg, MidiMessage::Channel {
    ///     channel: 0,
    ///     msg: ChannelMessage::NoteOn {
    ///         key: 64,
    ///         vel: 32,
    ///     },
    /// });
    /// ```
    ///
    /// ```
    /// # use midly_core::MidiMessage;
    /// let bytes = &[0x2A];
    /// let msg = MidiMessage::decode(bytes);
    ///
    /// assert_eq!(msg, MidiMessage::Unspecified([42][..].into()));
    /// ```
    #[inline]
    pub fn decode(data: &'a [u8]) -> MidiMessage<'a> {
        Self::try_decode(data).unwrap_or_else(|| MidiMessage::Unspecified(data.into()))
    }

    /// Encodes `self` as MIDI bytes, without doing any allocations.
    /// Must be supplied with a small scratch buffer for fixed-size messages.
    ///
    /// Returns an empty slice for normally unencodeable messages, such as meta-messages.
    ///
    /// # Examples
    ///
    /// ```
    /// # use midly_core::{MidiMessage, ChannelMessage};
    /// let msg = MidiMessage::Channel {
    ///     channel: 0,
    ///     msg: ChannelMessage::NoteOn {
    ///         key: 64,
    ///         vel: 32,
    ///     },
    /// };
    /// let mut buf = [0; 3];
    /// let bytes = msg.encode(&mut buf);
    ///
    /// assert_eq!(bytes, &[0x90, 0x40, 0x20]);
    /// ```
    ///
    /// ```
    /// # use midly_core::MidiMessage;
    /// let msg = MidiMessage::Tempo(9600);
    /// let mut buf = [0; 3];
    /// let bytes = msg.encode(&mut buf);
    ///
    /// assert_eq!(bytes, &[]);
    /// ```
    #[inline]
    pub fn encode<'b: 'a>(&'b self, buf: &'b mut [u8; 3]) -> &'b [u8] {
        use self::MidiMessage::*;
        let len;
        match self {
            MidiMessage::Channel { channel, msg } => {
                use self::ChannelMessage::*;
                match *msg {
                    NoteOff { key, vel } => {
                        *buf = [0x80 | channel, key, vel];
                        len = 3;
                    }
                    NoteOn { key, vel } => {
                        *buf = [0x90 | channel, key, vel];
                        len = 3;
                    }
                    Aftertouch { key, vel } => {
                        *buf = [0xA0 | channel, key, vel];
                        len = 3;
                    }
                    Controller { controller, value } => {
                        *buf = [0xB0 | channel, controller, value];
                        len = 3;
                    }
                    ProgramChange { program } => {
                        *buf = [0xC0 | channel, program, 0];
                        len = 2;
                    }
                    ChannelAftertouch { vel } => {
                        *buf = [0xD0 | channel, vel, 0];
                        len = 2;
                    }
                    PitchBend { bend } => {
                        let bend = bend.as_u16();
                        *buf = [0xE0 | channel, (bend & 0x7F) as u8, (bend >> 7) as u8];
                        len = 3;
                    }
                }
            }
            SysEx(data) => {
                return &data[..];
            }
            Mtc(kind, val) => {
                *buf = [0xF1, (*kind as u8) << 4 | val, 0];
                len = 2;
            }
            SongPosition(v) => {
                *buf = [0xF2, *v as u8 & 0x7F, (*v >> 7) as u8 & 0x7F];
                len = 3;
            }
            SongSelect(v) => {
                *buf = [0xF3, *v as u8, 0];
                len = 2;
            }
            TuneRequest => {
                *buf = [0xF6, 0, 0];
                len = 1;
            }
            TimingClock => {
                *buf = [0xF8, 0, 0];
                len = 1;
            }
            Start => {
                *buf = [0xFA, 0, 0];
                len = 1;
            }
            Continue => {
                *buf = [0xFB, 0, 0];
                len = 1;
            }
            Stop => {
                *buf = [0xFC, 0, 0];
                len = 1;
            }
            ActiveSensing => {
                *buf = [0xFE, 0, 0];
                len = 1
            }
            Reset => {
                *buf = [0xFF, 0, 0];
                len = 1;
            }
            Unspecified(data) => {
                return &data[..];
            }
            _ => {
                // Meta messages
                len = 0;
            }
        }
        &buf[..len]
    }

    /// Classifies `self` into one of the defined MIDI message classes: channel, system or meta.
    ///
    /// Additionally, system messages are subclassified as system common and system realtime, and
    /// the special case of invalid `Unspecified` messages is handled through the `Invalid`
    /// classification.
    #[inline]
    pub fn classify(&self) -> MessageKind {
        use self::{MessageKind::*, MidiMessage::*};
        match self {
            MidiMessage::Channel { .. } => MessageKind::Channel,
            SysEx(..) => SystemCommon,
            Mtc(..) => SystemCommon,
            SongPosition(..) => SystemCommon,
            SongSelect(..) => SystemCommon,
            TuneRequest => SystemCommon,
            TimingClock => SystemRealtime,
            Start => SystemRealtime,
            Continue => SystemRealtime,
            Stop => SystemRealtime,
            ActiveSensing => SystemRealtime,
            Reset => SystemRealtime,
            Unspecified(data) => match data.get(0) {
                Some(status) => match *status {
                    0x80..=0xEF => MessageKind::Channel,
                    0xF0..=0xF7 => SystemCommon,
                    0xF8..=0xFF => SystemRealtime,
                    _ => Invalid,
                },
                None => Invalid,
            },

            TrackNumber(..) => Meta,
            Text(..) => Meta,
            Copyright(..) => Meta,
            TrackName(..) => Meta,
            InstrumentName(..) => Meta,
            Lyric(..) => Meta,
            Marker(..) => Meta,
            CuePoint(..) => Meta,
            ProgramName(..) => Meta,
            DeviceName(..) => Meta,
            MidiChannel(..) => Meta,
            MidiPort(..) => Meta,
            EndOfTrack => Meta,
            Tempo(..) => Meta,
            SmpteOffset(..) => Meta,
            TimeSignature(..) => Meta,
            KeySignature(..) => Meta,
            SequencerSpecific(..) => Meta,
            UnknownMeta { .. } => Meta,
        }
    }

    /// Returns the channel and inner channel message if this message is actually a channel message.
    #[inline]
    pub fn as_channel(&self) -> Option<(u8, ChannelMessage)> {
        match self {
            &MidiMessage::Channel { channel, msg } => Some((channel, msg)),
            _ => None,
        }
    }

    /// Returns `true` if this message is a channel message, associated to a particular channel.
    ///
    /// Mutually exclusive with `is_system` and `is_meta`.
    #[inline]
    pub fn is_channel(&self) -> bool {
        self.classify().is_channel()
    }

    /// Returns `true` if this message is a System Common or System Realtime message.
    ///
    /// Mutually exclusive with `is_channel` and `is_meta`.
    #[inline]
    pub fn is_system(&self) -> bool {
        self.classify().is_system()
    }

    /// Returns `true` if this message is a Meta-Message, only present in `.mid` files.
    ///
    /// Mutually exclusive with `is_channel` and `is_system`.
    #[inline]
    pub fn is_meta(&self) -> bool {
        self.classify().is_meta()
    }
}

/// A Channel Message, that is, a MIDI message associated to a particular channel.
/// These messages make the bulk of most MIDI data.
///
/// Note that all byte values should be in the `0..=127` range.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ChannelMessage {
    /// Indicates that the given key should stop playing.
    NoteOff { key: u8, vel: u8 },
    /// Indicates that the given key should start playing with the given velocity.
    NoteOn { key: u8, vel: u8 },
    /// Indicates that the playing velocity (or intensity) of a given key has changed.
    Aftertouch { key: u8, vel: u8 },
    /// Changes the value of a particular MIDI controller.
    ///
    /// Refer to the MIDI spec for particular controller IDs.
    Controller { controller: u8, value: u8 },
    /// Indicates that the current channel should change program (also called instruments).
    ProgramChange { program: u8 },
    /// Indicates that the playing velocity (or intensity) of the entire channel has changed.
    ChannelAftertouch { vel: u8 },
    /// Indicates a new pitch bend for the entire channel.
    PitchBend { bend: PitchBend },
}

/// Indicates a particular pitch-bend value, stored as a signed value in the range
/// `PitchBend::MIN ..= PitchBend::MAX`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PitchBend(pub i16);
impl PitchBend {
    /// The minimum pitch-bend value, indicating full bend downwards.
    pub const MIN: i16 = -0x2000;
    /// The middle/zero pitch-bend value, indicating no bend.
    pub const MID: i16 = 0;
    /// The maximum pitch-bend value, indicating full bend upwards.
    pub const MAX: i16 = 0x1FFF;

    /// Convert the pitchbend value as an unsigned integer in the range `0..=0x3FFF`, where `0x2000`
    /// is the middle value.
    #[inline]
    pub const fn as_u16(&self) -> u16 {
        self.0.wrapping_sub(Self::MIN) as u16
    }

    /// Cast the pitchbend value as a signed integer in the range `-0x2000..=0x1FFF`, where `0` is
    /// the middle value.
    #[inline]
    pub const fn as_i16(&self) -> i16 {
        self.0
    }

    /// Cast the pitchbend value as a floating-point value in the range `-1.0 ..= 1.0`, where `0.0`
    /// is the middle value.
    #[inline]
    pub fn as_f32(&self) -> f32 {
        self.as_i16() as f32 * (-1. / Self::MIN as f32)
    }

    /// Cast the pitchbend value as a floating-point value in the range `-1.0 ..= 1.0`, where `0.0`
    /// is the middle value.
    #[inline]
    pub fn as_f64(&self) -> f64 {
        self.as_i16() as f64 * (-1. / Self::MIN as f64)
    }

    /// Convert from an unsigned integer in the range `0..=0x3FFF`, where `0x2000` is the middle
    /// value.
    #[inline]
    pub const fn from_u16(uint: u16) -> PitchBend {
        Self((uint as i16).wrapping_add(Self::MIN))
    }

    /// Convert from a signed integer in the range `-0x2000..=0x1FFF`, where `0` is the middle
    /// value.
    #[inline]
    pub const fn from_i16(int: i16) -> PitchBend {
        Self(int)
    }

    /// Convert from a floating-point value in the range `-1.0 ..= 1.0`, where `0.0` is the middle
    /// value.
    #[inline]
    pub fn from_f32(float: f32) -> PitchBend {
        Self::from_i16(((float * (-Self::MIN as f32)) as i16).clamp(Self::MIN, Self::MAX))
    }

    /// Convert from a floating-point value in the range `-1.0 ..= 1.0`, where `0.0` is the middle
    /// value.
    #[inline]
    pub fn from_f64(float: f64) -> PitchBend {
        Self::from_i16(((float * (-Self::MIN as f64)) as i16).clamp(Self::MIN, Self::MAX))
    }
}

/// A classification of a `MidiMessage`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MessageKind {
    /// A channel message, associated to a particular MIDI channel.
    Channel,
    /// A System Common message.
    SystemCommon,
    /// A System Realtime message, usually only sent in realtime, live connections.
    SystemRealtime,
    /// A Meta message, only present in static `.mid` files.
    Meta,
    /// A sequence of bytes that is not a valid MIDI message.
    Invalid,
}
impl MessageKind {
    /// Returns `true` if `self` is `Channel`.
    #[inline]
    pub fn is_channel(&self) -> bool {
        match self {
            MessageKind::Channel => true,
            _ => false,
        }
    }

    /// Returns `true` if `self` is `SystemCommon` or `SystemRealtime`.
    #[inline]
    pub fn is_system(&self) -> bool {
        match self {
            MessageKind::SystemCommon | MessageKind::SystemRealtime => true,
            _ => false,
        }
    }

    /// Returns `true` if `self` is `Meta`.
    #[inline]
    pub fn is_meta(&self) -> bool {
        match self {
            MessageKind::Meta => true,
            _ => false,
        }
    }
}

/// Represents a particular FPS from a set of valid FPS values, as indicated by the MIDI spec.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Fps {
    /// `24` frames per second.
    Fps24,
    /// `25` frames per second.
    Fps25,
    /// `29.97 = 30/1.001` frames per second.
    Fps2997,
    /// `30` frames per second.
    Fps30,
}
impl Fps {
    /// Cast the lower 2 bits of the given byte into an `Fps` value:
    /// - 0b00 => 24 fps
    /// - 0b01 => 25 fps
    /// - 0b10 => 29.97 fps
    /// - 0b11 => 30 fps
    #[inline]
    pub fn from_bits(bits: u8) -> Fps {
        match bits & 0b11 {
            0 => Fps::Fps24,
            1 => Fps::Fps25,
            2 => Fps::Fps2997,
            _ => Fps::Fps30,
        }
    }

    /// The inverse of `from_bits`.
    #[inline]
    pub fn as_bits(&self) -> u8 {
        *self as u8
    }
}

/// Used to tell which part of the MIDI Time Code Quarter Frame does the nibble payload in a
/// [`MidiMessage::Mtc`] message belong to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum MtcKind {
    /// The lower nibble of the frames.
    FramesLo,
    /// The higher nibble of the frames.
    FramesHi,
    /// The lower nibble of the seconds.
    SecondsLo,
    /// The higher nibble of the seconds.
    SecondsHi,
    /// The lower nibble of the minutes.
    MinutesLo,
    /// The higher nibble of the minutes.
    MinutesHi,
    /// The lower nibble of the hours.
    HoursLo,
    /// The higher nibble of the hours.
    HoursHi,
}
impl MtcKind {
    /// Cast this `MtcKind` into a single 3-bit integer, according to the following rules:
    ///
    /// - `FramesLo` => 0
    /// - `FramesHi` => 1
    /// - `SecondsLo` => 2
    /// - `SecondsHi` => 3
    /// - `MinutesLo` => 4
    /// - `MinutesHi` => 5
    /// - `HoursLo` => 6
    /// - `HoursHi` => 7
    #[inline]
    pub fn as_bits(&self) -> u8 {
        *self as u8
    }

    /// Inverse of [`MtcKind::as_bits()`].
    #[inline]
    pub fn from_bits(bits: u8) -> MtcKind {
        use self::MtcKind::*;
        match bits & 0b111 {
            0 => FramesLo,
            1 => FramesHi,
            2 => SecondsLo,
            3 => SecondsHi,
            4 => MinutesLo,
            5 => MinutesHi,
            6 => HoursLo,
            _ => HoursHi,
        }
    }
}

/// Represents a MIDI Time Code SMPTE Time, as specified in the MIDI specification.
///
/// This struct is rarely used, and many of its fields are even more rarely used.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SmpteTime {
    pub hour: u8,
    pub minute: u8,
    pub second: u8,
    pub frame: u8,
    pub subframe: u8,
    pub fps: Fps,
    pub color_frame: bool,
    pub negative: bool,
    pub has_status: bool,
}
impl SmpteTime {
    /// Create a full SMPTE timestamp from all 5 bytes.
    #[inline]
    pub fn from_bits_full(hour: u8, minute: u8, second: u8, frame: u8, subframe: u8) -> SmpteTime {
        SmpteTime {
            fps: Fps::from_bits(hour >> 5),
            hour: hour & 0b11111,
            color_frame: minute & 0b1000000 != 0,
            minute: minute & 0b111111,
            second: second & 0b111111,
            negative: frame & 0b1000000 != 0,
            has_status: frame & 0b100000 != 0,
            frame: frame & 0b11111,
            subframe: subframe & 0x7F,
        }
    }

    /// Create a partial SMPTE timestamp from just 4 bytes.
    #[inline]
    pub fn from_bits(hour: u8, minute: u8, second: u8, frame: u8) -> SmpteTime {
        Self::from_bits_full(hour, minute, second, frame, 0)
    }

    /// Cast the SMPTE timestamp as a full 5-byte timestamp.
    #[inline]
    pub fn as_bits_full(&self) -> [u8; 5] {
        let bits = self.as_bits();
        [bits[0], bits[1], bits[2], bits[3], self.subframe]
    }

    /// Cast the SMPTE timestamp as a partial 4-byte timestamp.
    #[inline]
    pub fn as_bits(&self) -> [u8; 4] {
        [
            self.hour | (self.fps.as_bits() << 5),
            ((self.color_frame as u8) << 6) | self.minute,
            self.second,
            ((self.negative as u8) << 6) | ((self.has_status as u8) << 5) | self.frame,
        ]
    }
}
