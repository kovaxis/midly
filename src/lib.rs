//! # Overview
//!
//! `midly` is a Standard Midi File (SMF) parser focused on speed and flexibility, parsing
//! multi-MB files in tenths of a second.
//!
//! Usage is as simple as:
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
//! The [`Smf`](struct.Smf.html) struct is the main type in the crate.
//! See its documentation for the structure of parsed MIDI files.
//!
//! # About lifetimes
//!
//! The `Smf` struct is used to store a parsed Standard Midi File (.mid and .midi files).
//! Notice that it has a lifetime parameter, since it stores references to the raw file bytes in
//! order to avoid allocations.
//! For this reason, the byte buffer must be created separately from the `Smf` structure:
//!
//! ```rust
//! # #[cfg(feature = "alloc")] {
//! use midly::Smf;
//!
//! // Load bytes into a buffer
//! let bytes = include_bytes!("../test-asset/Clementi.mid");
//!
//! // Parse file in a separate step
//! let smf = Smf::parse(bytes).unwrap();
//! # }
//! ```
//!
//! When loading a file something similar has to be done:
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
//! Saving SMF files is as simple as using the `Smf::save` method:
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
//! # About features
//!
//! The mode in which the crate works is configurable through the use of cargo features.
//! Two optional features are available: `std` and `strict`.
//! Only `std` is enabled by default.
//!
//! - The `std` feature
//!
//!   This feature enables the MIDI writer (which uses `std::io::Write`) and automatic
//!   parallelization for the `Smf::parse` and `Smf::parse_with_bytemap` functions (through the
//!   `rayon` dependency).
//!
//!   This feature is enabled by default. Disabling this feature with `default-features = false`
//!   will make the crate `no_std + alloc`.
//!
//! - The `strict` feature
//!
//!   By default `midly` will attempt to plow through non-standard and even obviously corrupted
//!   files, throwing away any unreadable data, or even entire tracks in the worst scenario.
//!   By enabling the `strict` feature the parser will reject SMF uncompliant files and do
//!   additional checking, throwing errors of the kind `ErrorKind::Malformed` when such a
//!   situation arises.
//!
//! # About generics
//!
//! The `Smf` type is generic over `T`, a type implementing [`TrackRepr`](trait.TrackRepr.html).
//! This `T` indicates how should each track be represented in memory.
//!
//! The default is `Vec<Event>`, produced by the `Smf::parse` method, but there are also two
//! other methods: [`Smf::parse_with_bytemap`](struct.Smf.html#method.parse_with_bytemap) and
//! [`Smf::parse_lazy`](struct.Smf.html#method.parse_lazy).
//! Check the documentation for these methods for more information about each.
//!
//! # Parsing raw MIDI streams
//!
//! The MIDI standard is independent from the Standard Midi File standard, even though the latter
//! depends on the former.
//! A raw, non-SMF MIDI streams would be a MIDI keyboard connected through USB for example.
//!
//! `midly` provides partial support for parsing these MIDI messages, through the
//! [`EventKind::parse`](enum.EventKind.html#method.parse) method, however most System Common
//! and System Realtime messages are unsupported.

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

mod prelude {
    pub(crate) use crate::{
        error::{ErrorKind, Result, ResultExt, StdResult},
        io::{IoResult, Seek, Write, WriteCounter},
        primitive::{u14, u24, u28, u4, u7, IntRead, IntReadBottom7, SplitChecked},
    };
    #[cfg(feature = "alloc")]
    pub(crate) use alloc::vec::Vec;
    pub(crate) use core::{convert::TryFrom, marker::PhantomData, mem, ops};
    #[cfg(feature = "std")]
    pub(crate) use std::{fs::File, io, path::Path};

    pub(crate) fn bit_range<T>(val: T, range: ops::Range<u32>) -> T
    where
        T: From<u8>
            + ops::Shr<u32, Output = T>
            + ops::Shl<u32, Output = T>
            + ops::Not<Output = T>
            + ops::BitAnd<Output = T>,
    {
        let mask = !((!T::from(0)) << (range.end - range.start));
        (val >> range.start) & mask
    }
}

mod event;
pub mod io;
mod primitive;
mod riff;
mod smf;

#[cfg(feature = "alloc")]
pub use crate::smf::{Smf, SmfBytemap};
pub use crate::{
    error::{Error, ErrorKind, Result},
    event::{
        Event, EventKind, MetaMessage, MidiMessage, MidiStream, MtcQuarterFrameMessage,
        StreamEvent, SystemCommon, SystemRealtime,
    },
    primitive::{Format, Fps, SmpteTime, Timing},
    smf::{parse, write, EventIter, Header, TrackIter},
};

/// Exotically-sized integers used by the MIDI standard.
pub mod num {
    pub use crate::primitive::{u14, u15, u24, u28, u4, u7};
}

#[cfg(test)]
mod test;
