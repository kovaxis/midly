//! # Examples
//!
//! The `Smf` struct is used to store a parsed Standard Midi File (.mid and .midi files).
//! Notice that it has a lifetime parameter, since it stores references to the raw file bytes.
//! For this reason, the byte buffer must be created separately to the `Smf` structure:
//!
//! ```rust
//! use midly::{Smf,Event};
//! //Load bytes into a buffer
//! let bytes=include_bytes!("../test-asset/Clementi.mid");
//! //Parse file in a separate step
//! let smf: Smf<Vec<Event>>=Smf::read(bytes).unwrap();
//! ```
//!
//! However, preloading into a buffer and dealing with generics can be tedious.
//! For this purposes, use `SmfBuffer` instead, which provides several `parse_*` non-generic
//! methods.
//!
//! ```rust
//! use midly::SmfBuffer;
//! //Load bytes into a buffer
//! let smf=SmfBuffer::open("test-asset/Clementi.mid").unwrap();
//! //Parse bytes in a separate step
//! //When in doubt, use parse_collect!
//! let smf=smf.parse_collect().unwrap();
//! ```
//!
//! Check the documentation for `SmfBuffer` for more information on the different `parse_*`
//! methods.

/// All of the errors this crate produces.
mod error {
    pub type Error = failure::Error;
    pub type Result<T> = std::result::Result<T, Error>;
}

mod prelude {
    pub use crate::{
        error::*,
        primitive::{u14, u15, u2, u24, u28, u4, u7, IntRead, IntReadBottom7, SplitChecked},
    };
    pub use bit::BitIndex;
    pub use failure::{bail, ensure, err_msg, ResultExt};
    pub use std::marker::PhantomData;
}

/// All sort of events and their parsing.
mod event;
/// Simple building-block data that can be read in one go.
/// All are stored in a fixed size (`Sized`) representation.
/// Also, primitives advance the file pointer when read.
mod primitive;
/// Parsing for SMF files, chunks and tracks.
mod smf;

pub use crate::{
    error::Error,
    event::{Event, EventKind, MetaMessage, MidiMessage},
    primitive::{Format, Fps, SmpteTime, Timing},
    smf::{Header, Smf, SmfBuffer, TrackIter},
};

/// Special-length integers used by the MIDI standard.
pub mod number {
    pub use crate::primitive::{u14, u15, u24, u28, u4, u7};
}

#[cfg(test)]
mod test;
