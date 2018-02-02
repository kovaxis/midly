//! # Usage
//!
//! The `Smf` `struct` is used to store a parsed Standard Midi File.
//! Note that it has a lifetime, since it references the original bytes for the file in order to not allocate memory.
//! For this reason, the byte buffer must outlive the `Smf` structure:
//! ```
//! use midly::Smf;
//! let bytes=include_bytes!("test-asset/Clementi.mid");
//! let smf=Smf::read(bytes).unwrap();
//! ```
//!
//! To ease loading files from the filesystem and dealing with generics, use `SmfBuffer` instead:
//! ```
//! let smf=SmfBuffer::open("test-asset/Clementi.mid");
//! let smf=smf.parse_collect().unwrap();
//! ```
//! Check the documentation for `SmfBuffer` for more information on `parse_*` methods.

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
pub mod number {
  pub use primitive::{u4,u7,u14,u15,u24,Varlen as VarlenInt};
}

#[cfg(test)]
mod test;