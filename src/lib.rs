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
//! For this purposes, use `SmfBuffer` instead, which provides several `parse_*` non-generic methods.
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
//! Check the documentation for `SmfBuffer` for more information on the different `parse_*` methods.

#[macro_use]
extern crate error_chain;
extern crate bit;
extern crate rayon;

///All errors this crate produces
mod error {
  error_chain!{}
}

mod prelude {
  pub use error::*;
  pub use primitive::{Varlen,u2,u4,u7,u14,u15,u24,IntRead,IntReadBottom7,SplitChecked};
  pub use bit::BitIndex;
  pub use std::marker::PhantomData;
}

///Parsing for SMF files, chunks and tracks
mod smf;
///All sort of events and their parsing
mod event;
///Simple building-block data that can be read in one go.
///All are stored in a fixed size (`Sized`) representation
///Also, primitives advance the file pointer when read
mod primitive;

pub use error::Error;
pub use smf::{SmfBuffer,Smf,TrackIter,Header};
pub use event::{Event,EventKind,MidiMessage,MetaMessage};
pub use primitive::{Format,Timing,SmpteTime,Fps};
///Many kinds of numbers upholding different guarantees about their contents.
pub mod number {
  pub use primitive::{u4,u7,u14,u15,u24,Varlen as VarlenInt};
}

#[cfg(test)]
mod test;