//! # Overview
//!
//! `midly_smf` is a full-featured MIDI parser and writer with a strong focus on performance.
//!
//! Parsing a `.mid` file can be as simple as:
//!
//! ```
//! # #[cfg(feature = "alloc")] {
//! use midly_smf::Smf;
//!
//! # fn read_file(_: &str) -> Vec<u8> {
//! #     std::fs::read("test-asset/Clementi.mid").unwrap()
//! # }
//!
//! let mut raw_bytes: Vec<u8> = read_file("Pi.mid");
//! let smf = Smf::parse(&mut raw_bytes).unwrap();
//!
//! for (i, track) in smf.tracks.iter().enumerate() {
//!     println!("track {} has {} events", i, track.len());
//! }
//! # }
//! ```
//!
//! # Parsing Standard Midi Files (also known as `.mid` files)
//!
//! Parsing Standard Midi Files is usually done through the [`Smf`] struct (or if
//! working in a `no_std` environment without an allocator, through the [`parse()`] function).
//!
//! Note that most types in this crate have a lifetime parameter, because they perform zero-copy
//! parsing as often as possible, and reference the bytes in the original file (in order to avoid
//! allocations).
//! For this reason, reading and parsing a file is done in two separate steps:
//!
//! ```
//! # #[cfg(feature = "std")] {
//! use std::fs;
//! use midly::Smf;
//!
//! // Load bytes into a buffer
//! let mut bytes = fs::read("test-asset/Clementi.mid").unwrap();
//!
//! // Parse bytes in a separate step
//! let smf = Smf::parse(&mut bytes).unwrap();
//! # }
//! ```
//!
//! If this is unacceptable, the [`Smf::into_owned()`] method can turn borrowed data into owned
//! data, removing lifetime restrictions at some extra cost:
//!
//! ```
//! # #[cfg(feature = "std")] {
//! # use std::fs;
//! # use midly::Smf;
//!
//! // Load and parse in a single step
//! let smf = Smf::parse(
//!     &mut fs::read("test-asset/Clementi.mid").unwrap()
//! ).unwrap().into_owned();
//! # }
//! ```
//!
//! # Writing Standard Midi Files
//!
//! Saving `.mid` files is as simple as using the [`Smf::save()`] method:
//!
//! ```
//! # #[cfg(feature = "std")] {
//! # use std::fs;
//! # use midly::Smf;
//! // Parse file
//! let mut bytes = fs::read("test-asset/Clementi.mid").unwrap();
//! let smf = Smf::parse(&mut bytes).unwrap();
//!
//! // Rewrite file
//! smf.save("test-asset/ClementiRewritten.mid").unwrap();
//! # }
//! ```
//!
//! SMF files can also be written to an arbitrary writer:
//!
//! ```
//! # #[cfg(feature = "std")] {
//! # use std::fs;
//! # use midly::Smf;
//! # let mut bytes = fs::read("test-asset/Clementi.mid").unwrap();
//! # let smf = Smf::parse(&mut bytes).unwrap();
//! let mut in_memory = Vec::new();
//! smf.write(&mut in_memory).unwrap();
//!
//! println!("midi file fits in {} bytes!", in_memory.len());
//! # }
//! ```
//!
//! # Iterating over MIDI events in a file
//!
//! Since MIDI files can be split into several parallel tracks, iterating over events in order,
//! while keeping track of the interval between events can be tricky. This crate implements the
//! necessary logic to iterate over events in any kind of `.mid` file type: simply choose one of the
//! `iter_*()` methods of the [`Smf`] type:
//!
//! ```
//! # #[cfg(feature = "alloc")] {
//! # use std::fs;
//! # use midly::Smf;
//! # let mut bytes = fs::read("test-asset/Clementi.mid").unwrap();
//! # let smf = Smf::parse(&mut bytes).unwrap();
//! use std::{time::Duration, thread};
//!
//! for (delay, msg) in smf.timed_iter() {
//!     thread::sleep(delay);
//!     println!("played {:?}", msg);
//! }
//! # }
//! ```
//!
//! If you are looking to play back a MIDI file, you might be interested in the umbrella
//! [`midly`](https://lib.rs/midly) crate.
//!
//! # Parsing and writing standalone MIDI messages
//!
//! To simply parse a slice of bytes into a single MIDI message, or convert a MIDI message into its
//! corresponding slice of bytes, use the `midly_core` crate, which comes with this functionality
//! `built-in` in a much leaner package.
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
//!   This feature enables integration with `std`, for example implementing [`std::error::Error`]
//!   for [`Error`], support for writing to [`std::io::Write`] streams, among others.
//!
//!   Disabling this feature will make the crate `no_std`.
//!
//! - `alloc` (enabled by default)
//!
//!   This feature enables allocations both for ergonomics and performance.
//!
//!   Disabling both the `std` and the `alloc` feature will make the crate fully `no_std`, but will
//!   reduce functionality to a minimum.
//!   For example, the [`Smf`] type is unavailable without the `alloc` feature.
//!   All types that are unavailable when a feature is disabled are marked as such in their
//!   documentation.
//!
//! - `strict`
//!
//!   By default `midly` will attempt to plow through non-standard and even obviously corrupted
//!   files, throwing away any unreadable data, or even entire tracks in the worst scenario.
//!   By enabling the `strict` feature the parser will reject uncompliant data and do
//!   additional checking, throwing errors of the kind [`ErrorKind::Malformed`] when such a
//!   situation arises.

#![cfg_attr(not(any(test, feature = "std")), no_std)]

#[cfg(feature = "alloc")]
extern crate alloc;

mod prelude {
    pub(crate) use crate::{
        error::{ErrorKind, Result, ResultExt},
        io::{Seek, Write, WriteCounter, WriteResult},
        primitive::{
            advance_slice, read_slice, read_u16, read_u32, read_u8, read_varlen,
            read_varlen_noconsume, read_varlen_slice, write_varlen, write_varlen_slice,
        },
    };
    #[cfg(feature = "alloc")]
    pub use alloc::{vec, vec::Vec};
    pub use core::{
        convert::{TryFrom, TryInto},
        fmt,
        marker::PhantomData,
        mem::{self, MaybeUninit as Uninit},
        ptr,
        result::Result as StdResult,
        time::Duration,
    };
    pub use midly_core::MidiMessage;
    #[cfg(feature = "std")]
    pub use std::{error::Error as StdError, fs::File, io, path::Path};
}

#[macro_use]
mod error;

mod event;
pub mod io;
#[cfg(feature = "alloc")]
mod playback;
mod primitive;
mod riff;
mod smf;

#[cfg(feature = "std")]
pub use crate::smf::write_std;
#[cfg(feature = "alloc")]
pub use crate::smf::{Smf, Track};
pub use crate::{
    error::{Error, ErrorKind, Result},
    event::TrackEvent,
    smf::{parse, write, Format, Header, Timing},
};
pub use midly_core;
pub mod iter {
    pub use crate::playback::{
        OrderedIntoIter, OrderedIter, OrderedIterMut, TimedIntoIter, TimedIter, TimedIterMut,
    };
    pub use crate::smf::{EventIter, TrackIter};
}

#[cfg(test)]
mod test;
