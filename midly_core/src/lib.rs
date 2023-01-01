//! Defines common MIDI types for other crates to use, in a way that is both flexible and
//! performant.
//!
//!
//!
//! # Parsing standalone MIDI messages
//!
//! To parse a slice of MIDI bytes into a single MIDI message, use the [`MidiMessage::decode()`]
//! function:
//!
//! ```
//! use midly_core::{MidiMessage, ChannelMessage};
//!
//! fn on_midi(bytes: &[u8]) {
//!     let msg = MidiMessage::decode(bytes);
//!     match msg {
//!         MidiMessage::Channel { channel, msg } => match msg {
//!             ChannelMessage::NoteOn { key, vel } => {
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
//! To convert a MIDI message into its standard MIDI-bytes representation, use the
//! [`MidiMessage::encode()`] method:
//!
//! ```
//! use midly_core::{MidiMessage, ChannelMessage};
//! # fn write_midi(_bytes: &[u8]) {}
//!
//! fn note_on(channel: u8, key: u8) {
//!     let msg = MidiMessage::Channel {
//!         channel: channel,
//!         msg: ChannelMessage::NoteOn {
//!             key,
//!             vel: 127,
//!         },
//!     };
//!     let mut buf = [0; 3];
//!     let bytes: &[u8] = msg.encode(&mut buf);
//!     write_midi(bytes);
//! }
//! # note_on(3, 61); note_on(2, 50); note_on(2,61);
//! ```
//!
//! # `no_std` support
//!
//! In order to enable `no_std` support, the `std` feature must be disabled (using the
//! `no-default-features` cargo flag).
//!
//! There is currently no `std` integration, but this might change in the future, so the `std`
//! feature **must** be disabled to make the crate `no_std`.
//!
//! Additionally, if no dynamic allocation is available, the `alloc` feature can also be disabled.
//! This will remove many methods that depend on global allocation, but will leave all types
//! unchanged.

#![cfg_attr(not(any(feature = "std", test)), no_std)]

#[cfg(feature = "alloc")]
extern crate alloc;

mod prelude {
    pub(crate) use crate::bytes::Bytes;
    pub use core::{
        convert::{TryFrom, TryInto},
        fmt,
        marker::PhantomData,
        mem, ops,
        ptr::{self, NonNull},
    };
}

mod bytes;
mod midi;

pub use crate::{
    bytes::{Bytes, ModifyBytes},
    midi::{ChannelMessage, Fps, MidiMessage, MtcKind, PitchBend, SmpteTime},
};

#[cfg(test)]
mod test;
