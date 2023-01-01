//! Provides support for the niche use case of reading MIDI events from a non-delimited stream.
//!
//! Low level drivers or embedded devices might have access to raw MIDI streams, which are just a
//! stream of bytes that might arrive at any rate.
//!
//! For this use case, the `MidiStream` type is provided, which can receive streaming bytes and
//! produces MIDI messages.
//!
//! Handles all of the quirks specific to MIDI streams, including System Realtime messages embedded
//! in the middle of another message.

use crate::{
    event::MidiMessage,
    live::{LiveEvent, SystemRealtime},
    prelude::*,
};

/// A streaming raw MIDI parser.
/// This parser takes raw MIDI, *not* `.midi` files!
///
/// Read the module documentation for more info.
#[derive(Clone, Debug, Default)]
pub struct MidiStream<B = DefaultBuffer> {
    status: Option<u8>,
    data: B,
}
impl MidiStream {
    /// Create a new clean midi stream with the default buffer size.
    #[inline]
    pub fn new() -> MidiStream {
        MidiStream::default()
    }
}
impl<B: Buffer> MidiStream<B> {
    /// Create a new clean midi stream using the given data buffer.
    #[inline]
    pub fn with_buffer(mut buf: B) -> MidiStream<B> {
        buf.clear();
        MidiStream {
            status: None,
            data: buf,
        }
    }

    #[inline]
    fn event(&mut self, status: u8, mut handle_ev: impl FnMut(LiveEvent)) {
        if let Ok(ev) = LiveEvent::read(status, self.data.as_slice()) {
            handle_ev(ev);
        }
    }

    #[inline]
    fn feed_byte(&mut self, byte: u8, mut handle_ev: impl FnMut(LiveEvent)) {
        if let Some(byte) = u7::try_from(byte) {
            //Data byte
            if let Some(status) = self.status {
                match self.data.push(&[byte]) {
                    Ok(()) => {
                        //Midi messages have a known length, so when a data byte beyond the fixed
                        //message length arrives, we must know to finish off the previous message
                        //and start a new one sharing the status of the previous byte (running
                        //status).
                        //Besides, we'd like to trigger MIDI messages as soon as the necessary data
                        //arrives.
                        //To solve both of these issues, events are triggered as soon as their
                        //data quota is fulfilled.
                        //Note that if `msg_length` returns 0, this `if` will never execute, since
                        //at this point the length of `self.data` is at least 1 (since a data byte
                        //was just pushed).
                        if self.data.as_slice().len() == MidiMessage::msg_length(status) {
                            self.event(status, handle_ev);
                            self.data.clear();
                        }
                    }
                    Err(()) => {
                        //Data for this message is too long, drop it
                        self.status = None;
                        self.data.clear();
                    }
                }
            }
        } else {
            //Status byte
            if let 0xF8..=0xFF = byte {
                //System Realtime
                //These single-byte events are intended to transmit quick time-sensitive events,
                //and they should be invisible to other messages (that means, they don't alter any
                //decoder state).
                //They can appear in between the status and data bytes of other messages, and even
                //in between the data bytes of other messages.
                handle_ev(LiveEvent::Realtime(SystemRealtime::new(byte)));
            } else {
                //Channel/System Common
                //Because the status is about to be cleared, process the previous one
                if let Some(status) = self.status {
                    self.event(status, handle_ev);
                }
                //Set the new status
                self.status = Some(byte);
                self.data.clear();
            }
        }
    }

    /// Feeds a slice of bytes to the stream, calling the `handle_ev` closure whenever a complete
    /// event is read.
    ///
    /// Calling `feed` with many small slices is equivalent to calling `feed` with one large
    /// concatenation of them all.
    ///
    /// Note that some calls to `feed` might produce no events, and others may produce as many as
    /// the amount of bytes fed in.
    pub fn feed(&mut self, bytes: &[u8], mut handle_ev: impl FnMut(LiveEvent)) {
        for &byte in bytes {
            self.feed_byte(byte, &mut handle_ev);
        }
    }

    /// Indicates to the stream that this is an event boundary, such as for example when the stream
    /// is closed.
    /// Not calling this function might drop some pending events.
    pub fn flush(&mut self, handle_ev: impl FnMut(LiveEvent)) {
        if let Some(status) = self.status.take() {
            self.event(status, handle_ev);
            self.status = None;
            self.data.clear();
        }
    }
}

/// Describes types that can be used as data buffers for the [`MidiStream`](struct.MidiStream.html)
/// type.
///
/// This trait is automatically implemented by the [`stack_buffer!`](../macro.stack_buffer.html)
/// macro, and very rarely should be implemented manually.
pub trait Buffer {
    /// Add a new slice of data to the tail of the buffer.
    fn push(&mut self, data: &[u7]) -> StdResult<(), ()>;
    /// Clear the entire buffer.
    fn clear(&mut self);
    /// Present the buffer contents as a slice of 7-bit units.
    fn as_slice(&self) -> &[u7];
}

/// A `Buffer` with virtually unlimited capacity.
#[cfg(feature = "alloc")]
impl Buffer for Vec<u7> {
    #[inline]
    fn push(&mut self, data: &[u7]) -> StdResult<(), ()> {
        self.extend_from_slice(data);
        Ok(())
    }
    #[inline]
    fn clear(&mut self) {
        Vec::clear(self)
    }
    #[inline]
    fn as_slice(&self) -> &[u7] {
        self
    }
}

/// Define a stack buffer type, suitable for use with [`MidiStream`](stream/struct.MidiStream.html).
///
/// # Usage
///
/// The `stack_buffer!` macro defines a buffer type, which can later be instatiated for use with
/// a `MidiStream`.
///
/// ```rust
/// midly::stack_buffer! {
///     struct MyBuffer([u8; 12345]);
/// }
///
/// use midly::stream::MidiStream;
/// let stream = MidiStream::with_buffer(MyBuffer::new());
/// ```
///
/// Buffers can have attributes, documentation, and be made `pub`lic.
///
/// ```rust
/// midly::stack_buffer! {
///     /// A very small buffer.
///     #[repr(C)]
///     pub struct MyBuffer([u8; 16]);
/// }
///
/// use midly::stream::MidiStream;
/// let stream = MidiStream::<MyBuffer>::default();
/// ```
#[macro_export]
macro_rules! stack_buffer {
    {
        @impl_def {$($attr:meta)*} {$($pub:ident)?} {$name:ident} {$size:expr}
    } => {
        $(#[$attr])*
        #[derive(Clone)]
        $($pub)? struct $name {
            buf: [$crate::num::u7; $size],
            len: usize,
        }
        impl core::hash::Hash for $name {
            #[inline]
            fn hash<H: core::hash::Hasher>(&self, h: &mut H) {
                h.write($crate::num::u7::slice_as_int(&self.buf[..self.len]));
                h.write(&[0xFF]);
            }
        }
        impl core::fmt::Debug for $name {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                write!(f, concat!(stringify!($name), "["))?;
                for databyte in self.buf.iter() {
                    write!(f, "{:02x}", databyte.as_int())?;
                }
                write!(f, "]")?;
                Ok(())
            }
        }
        impl $name {
            pub const MAX_CAP: usize = $size;
            #[inline]
            $($pub)? const fn new() -> $name {
                $name {
                    buf: [$crate::num::u7::new(0); $size],
                    len: 0,
                }
            }
        }
        impl core::default::Default for $name {
            #[inline]
            fn default() -> $name {
                Self::new()
            }
        }
        impl $crate::stream::Buffer for $name {
            #[inline]
            fn push(&mut self, data: &[$crate::num::u7]) -> core::result::Result<(), ()> {
                let new_len = self.len + data.len();
                if new_len > Self::MAX_CAP {
                    Err(())
                } else {
                    self.buf[self.len..new_len].copy_from_slice(data);
                    self.len = new_len;
                    Ok(())
                }
            }
            #[inline]
            fn clear(&mut self) {
                self.len = 0;
            }
            #[inline]
            fn as_slice(&self) -> &[$crate::num::u7] {
                &self.buf[..self.len]
            }
        }
    };
    {
        $(#[$attr:meta])*
        struct $name:ident([u8; $size:expr]);
    }=> {
        $crate::stack_buffer!(@impl_def {$($attr)*} {} {$name} {$size});
    };
    {
        $(#[$attr:meta])*
        pub struct $name:ident([u8; $size:expr]);
    }=> {
        $crate::stack_buffer!(@impl_def {$($attr)*} {pub} {$name} {$size});
    };
}

macro_rules! default_buffer_def {
    ($($item:item)*) => {
        /// The default buffer type used for [`MidiStream`](struct.MidiStream.html).
        /// By default it will have a reasonable maximum capacity, but the `Buffer` trait can be
        /// implemented for fine-grained control.
        ///
        /// # Implementation notes
        ///
        /// Currently, when the `alloc` feature is used a `Vec` is used for the backing allocation,
        /// limited to a maximum of 256KB.
        ///
        /// When the `alloc` feature is disabled a 16KB stack buffer is used instead.
        ///
        /// This implementation is subject to change at any time, including reductions in size.
        #[derive(Clone, Hash, Default)]
        $($item)*
    };
}
pub use self::default_buf_impl::DefaultBuffer;

#[cfg(feature = "alloc")]
mod default_buf_impl {
    use super::*;

    default_buffer_def! {
        pub struct DefaultBuffer {
            buf: Vec<u7>,
        }
    }

    impl fmt::Debug for DefaultBuffer {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "DefaultBuffer[")?;
            for databyte in self.buf.iter() {
                write!(f, "{:02x}", databyte.as_int())?;
            }
            write!(f, "]")?;
            Ok(())
        }
    }
    impl DefaultBuffer {
        const MAX_CAP: usize = 256 * 1024;
        /// Get the maximum capacity of this buffer.
        #[inline]
        pub const fn max_cap(&self) -> usize {
            Self::MAX_CAP
        }
        /// Create a new empty buffer.
        #[inline]
        pub const fn new() -> DefaultBuffer {
            DefaultBuffer { buf: Vec::new() }
        }
    }
    impl Buffer for DefaultBuffer {
        #[inline]
        fn push(&mut self, data: &[u7]) -> StdResult<(), ()> {
            if self.buf.len() + data.len() > Self::MAX_CAP {
                Err(())
            } else {
                self.buf.extend_from_slice(data);
                Ok(())
            }
        }
        #[inline]
        fn clear(&mut self) {
            self.buf.clear()
        }
        #[inline]
        fn as_slice(&self) -> &[u7] {
            &self.buf[..]
        }
    }
}

#[cfg(not(feature = "alloc"))]
mod default_buf_impl {
    use super::*;

    default_buffer_def! {
        pub struct DefaultBuffer {
            buf: InnerBuf,
        }
    }
    stack_buffer! {
        struct InnerBuf([u8; 16*1024]);
    }
    impl fmt::Debug for DefaultBuffer {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            fmt::Debug::fmt(&self.buf, f)
        }
    }
    impl DefaultBuffer {
        /// Get the maximum capacity of this buffer.
        #[inline]
        pub const fn max_cap(&self) -> usize {
            InnerBuf::MAX_CAP
        }
        /// Create a new empty buffer.
        #[inline]
        pub const fn new() -> DefaultBuffer {
            DefaultBuffer {
                buf: InnerBuf::new(),
            }
        }
    }
    impl Buffer for DefaultBuffer {
        #[inline]
        fn push(&mut self, data: &[u7]) -> StdResult<(), ()> {
            self.buf.push(data)
        }
        #[inline]
        fn clear(&mut self) {
            self.buf.clear()
        }
        #[inline]
        fn as_slice(&self) -> &[u7] {
            self.buf.as_slice()
        }
    }
}
