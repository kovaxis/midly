//! # Overview
//!
//! `midly` is a full-featured MIDI parser and writer, focused on performance.
//!
//! Parsing a `.mid` file can be as simple as:
//!
//! ```rust
//! # #[cfg(feature = "alloc")] {
//! use midly::Smf;
//!
//! let smf = Smf::parse(include_bytes!("../test-asset/Clementi.mid")).unwrap();
//!
//! for (i, track) in smf.tracks.iter().enumerate() {
//!     println!("track {} has {} events", i, track.len());
//! }
//! # }
//! ```
//!
//! # Parsing Standard Midi Files (`.mid` files)
//!
//! Parsing Standard Midi Files is usually done through the [`Smf`](struct.Smf.html) struct (or if
//! working in a `no_std` environment without an allocator, through the [`parse`](fn.parse.html)
//! function).
//!
//! Note that most types in this crate have a lifetime parameter, because they reference the bytes
//! in the original file (in order to avoid allocations).
//! For this reason, reading a file and parsing it must be done in two separate steps:
//!
//! ```rust
//! # #[cfg(feature = "alloc")] {
//! use std::fs;
//! use midly::Smf;
//!
//! // Load bytes into a buffer
//! let bytes = fs::read("test-asset/Clementi.mid").unwrap();
//!
//! // Parse bytes in a separate step
//! let smf = Smf::parse(&bytes).unwrap();
//! # }
//! ```
//!
//! # Writing Standard Midi Files
//!
//! Saving `.mid` files is as simple as using the `Smf::save` method:
//!
//! ```rust
//! # #[cfg(feature = "std")] {
//! # use std::fs;
//! # use midly::Smf;
//! // Parse file
//! let bytes = fs::read("test-asset/Clementi.mid").unwrap();
//! let smf = Smf::parse(&bytes).unwrap();
//!
//! // Rewrite file
//! smf.save("test-asset/ClementiRewritten.mid").unwrap();
//! # }
//! ```
//!
//! SMF files can also be written to an arbitrary writer:
//!
//! ```rust
//! # #[cfg(feature = "std")] {
//! # use std::fs;
//! # use midly::Smf;
//! # let bytes = fs::read("test-asset/Clementi.mid").unwrap();
//! # let smf = Smf::parse(&bytes).unwrap();
//! let mut in_memory = Vec::new();
//! smf.write(&mut in_memory).unwrap();
//!
//! println!("midi file fits in {} bytes!", in_memory.len());
//! # }
//! ```
//!
//! # Parsing standalone MIDI messages
//!
//! When using an OS API such as [`midir`](https://docs.rs/midir),
//! [`LiveEvent`](live/enum.LiveEvent.html) can be used to parse the raw MIDI bytes:
//!
//! ```rust
//! use midly::{live::LiveEvent, MidiMessage};
//!
//! fn on_midi(event: &[u8]) {
//!     let event = LiveEvent::parse(event).unwrap();
//!     match event {
//!         LiveEvent::Midi { channel, message } => match message {
//!             MidiMessage::NoteOn { key, vel } => {
//!                 println!("hit note {} on channel {}", key, channel);
//!             }
//!             _ => {}
//!         },
//!         _ => {}
//!     }
//! }
//! ```
//!
//! # Writing standalone MIDI messages
//!
//! Raw MIDI message bytes can be produced for consumption by OS APIs, such as
//! [`midir`](https://docs.rs/midir), through the
//! [`LiveEvent::write`](live/enum.LiveEvent.html#method.write) method:
//!
//! ```rust
//! use midly::{live::LiveEvent, MidiMessage};
//! # fn write_midi(bytes: &[u8]) {}
//!
//! fn note_on(channel: u8, key: u8) {
//!     let ev = LiveEvent::Midi {
//!         channel: channel.into(),
//!         message: MidiMessage::NoteOn {
//!             key: key.into(),
//!             vel: 127.into(),
//!         },
//!     };
//! #   let mut stack_buf = [0; 3];
//! #   let mut buf = {
//! #       #[cfg(feature = "alloc")] {
//!     let mut buf = Vec::new();
//! #           buf
//! #       }
//! #       #[cfg(not(feature = "alloc"))] {
//! #           &mut stack_buf[..]
//! #       }
//! #   };
//!     ev.write(&mut buf).unwrap();
//!     write_midi(&buf[..]);
//! }
//! # note_on(3, 61); note_on(2, 50); note_on(2,61);
//! ```
//!
//! # About features
//!
//! The following cargo features are available to enable or disable parts of the crate:
//!
//! - `parallel` (enabled by default)
//!
//!   This feature enables the use of multiple threads when parsing large midi files.
//!
//!   Disabling this feature will remove the dependency on `rayon`.
//!
//! - `std` (enabled by default)
//!
//!   This feature enables integration with `std`, for example implementing `std::error::Error` for
//!   [`midly::Error`](struct.Error.html), support for writing to `std::io::Write` streams, among
//!   others.
//!
//!   Disabling this feature will make the crate `no_std`.
//!
//! - `alloc` (enabled by default)
//!
//!   This feature enables allocations both for ergonomics and performance.
//!
//!   Disabling both the `std` and the `alloc` feature will make the crate fully `no_std`, but will
//!   reduce functionality to a minimum.
//!   For example, the [`Smf`](struct.Smf.html) type is unavailable without the `alloc` feature.
//!   All types that are unavailable when a feature is disabled are marked as such in their
//!   documentation.
//!
//! - `strict`
//!
//!   By default `midly` will attempt to plow through non-standard and even obviously corrupted
//!   files, throwing away any unreadable data, or even entire tracks in the worst scenario.
//!   By enabling the `strict` feature the parser will reject uncompliant data and do
//!   additional checking, throwing errors of the kind
//!   [`ErrorKind::Malformed`](enum.ErrorKind.html#variant.Malformed) when such a situation arises.

#![cfg_attr(not(any(test, feature = "std")), no_std)]

#[cfg(feature = "alloc")]
extern crate alloc;

macro_rules! bail {
    ($err:expr) => {{
        return Err($err.into());
    }};
}
macro_rules! ensure {
    ($cond:expr, $err:expr) => {{
        if !$cond {
            bail!($err)
        }
    }};
}

/// All of the errors this crate produces.
#[macro_use]
mod error;

#[macro_use]
mod prelude {
    #[cfg(feature = "std")]
    pub(crate) use crate::io::IoWrap;
    pub(crate) use crate::{
        error::{ErrorKind, Result, ResultExt, StdResult},
        io::{Seek, Write, WriteCounter, WriteResult},
        primitive::{u14, u24, u28, u4, u7, IntRead, IntReadBottom7, SplitChecked},
    };
    #[cfg(feature = "alloc")]
    pub(crate) use alloc::{boxed::Box, vec, vec::Vec};
    pub(crate) use core::{convert::TryFrom, fmt, marker::PhantomData, mem};
    #[cfg(feature = "std")]
    pub(crate) use std::{fs::File, io, path::Path};

    macro_rules! bit_range {
        ($val:expr, $range:expr) => {{
            let mask = (1 << ($range.end - $range.start)) - 1;
            ($val >> $range.start) & mask
        }};
    }
}

mod arena;
mod event;
pub mod io;
pub mod live;
mod primitive;
mod riff;
mod smf;
pub mod stream;

#[cfg(feature = "std")]
pub use crate::smf::write_std;
#[cfg(feature = "alloc")]
pub use crate::{
    arena::Arena,
    smf::{BytemappedTrack, Smf, SmfBytemap, Track},
};
pub use crate::{
    error::{Error, ErrorKind, Result},
    event::{MetaMessage, MidiMessage, PitchBend, TrackEvent, TrackEventKind},
    primitive::{Format, Fps, SmpteTime, Timing},
    smf::{parse, write, EventBytemapIter, EventIter, Header, TrackIter},
};

/// Exotically-sized integers used by the MIDI standard.
pub mod num {
    pub use crate::primitive::{u14, u15, u24, u28, u4, u7};
}

#[cfg(test)]
mod test;
