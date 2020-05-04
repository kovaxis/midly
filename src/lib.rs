//! # Overview
//!
//! `midly` is a Standard Midi File (SMF) parser focused on speed and flexibility, parsing
//! multi-MB files in tenths of a second.
//!
//! Usage is as simple as:
//!
//! ```rust
//! use midly::Smf;
//!
//! let smf = Smf::parse(include_bytes!("../test-asset/Clementi.mid")).unwrap();
//!
//! for (i, track) in smf.tracks.iter().enumerate() {
//!     println!("track {} has {} events", i, track.len());
//! }
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
//! use midly::Smf;
//!
//! // Load bytes into a buffer
//! let bytes = include_bytes!("../test-asset/Clementi.mid");
//!
//! // Parse file in a separate step
//! let smf = Smf::parse(bytes).unwrap();
//! ```
//!
//! When loading a file something similar has to be done:
//!
//! ```rust
//! use std::fs;
//! use midly::Smf;
//!
//! // Load bytes into a buffer
//! let bytes = fs::read("test-asset/Clementi.mid").unwrap();
//!
//! // Parse bytes in a separate step
//! let smf = Smf::parse(&bytes).unwrap();
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

#![forbid(unsafe_code)]
#![cfg_attr(not(any(test, feature = "std")), no_std)]

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
mod error {
    use core::fmt;
    use failure::Fail;

    #[cfg(all(debug_assertions, feature = "std"))]
    mod error_impl {
        use super::{Error, ErrorExt, ErrorKind};
        use failure::{Backtrace, Context, Fail};

        pub type ErrorInner = Context<ErrorKind>;
        impl ErrorExt for Error {
            fn kind(&self) -> ErrorKind {
                *self.inner.get_context()
            }
            fn chain_ctx(self, ctx: &'static ErrorKind) -> Error {
                Error {
                    inner: Fail::context(self, *ctx),
                }
            }
        }
        impl Fail for Error {
            fn cause(&self) -> Option<&dyn Fail> {
                self.inner.cause()
            }

            fn backtrace(&self) -> Option<&Backtrace> {
                self.inner.backtrace()
            }
        }
        impl From<&'static ErrorKind> for Error {
            fn from(kind: &'static ErrorKind) -> Error {
                Error {
                    inner: Context::new(*kind),
                }
            }
        }
    }

    #[cfg(not(all(debug_assertions, feature = "std")))]
    mod error_impl {
        use super::{Error, ErrorExt, ErrorKind};
        use failure::Fail;

        /// In release mode errors are just a thin pointer.
        pub type ErrorInner = &'static ErrorKind;
        impl Fail for Error {}
        impl ErrorExt for Error {
            fn kind(&self) -> ErrorKind {
                *self.inner
            }
            fn chain_ctx(self, ctx: &'static ErrorKind) -> Error {
                Error { inner: ctx }
            }
        }
        impl From<&'static ErrorKind> for Error {
            fn from(inner: &'static ErrorKind) -> Error {
                Error { inner }
            }
        }
    }

    /// Represents an error parsing an SMF file or MIDI stream.
    ///
    /// This type wraps an `ErrorKind` and includes backtrace and error chain data in debug mode.
    /// In release mode it is a newtype wrapper around `ErrorKind`, so the `Fail::cause` method
    /// always returns `None`.
    ///
    /// For more information about the error policy used by `midly`, see
    /// [`ErrorKind`](enum.ErrorKind.html).
    #[derive(Debug)]
    pub struct Error {
        inner: self::error_impl::ErrorInner,
    }
    impl Error {
        /// More information about the error itself.
        ///
        /// To traverse the causes of the error use the `Fail` trait instead.
        /// Note that error chains are only available in debug mode.
        pub fn kind(&self) -> ErrorKind {
            ErrorExt::kind(self)
        }
    }
    impl fmt::Display for Error {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            fmt::Display::fmt(&self.inner, f)
        }
    }

    trait ErrorExt {
        fn kind(&self) -> ErrorKind;
        fn chain_ctx(self, ctx: &'static ErrorKind) -> Error;
    }

    /// The type of error that occurred while parsing.
    ///
    /// As a library consumer, detailed errors about what specific part of the MIDI spec was
    /// violated are not very useful.
    /// For this reason, errors are broadly categorized into 2 classes, and specific error info is
    /// provided as a non-normative string literal.
    #[derive(Copy, Clone, Debug, Fail)]
    pub enum ErrorKind {
        /// Fatal errors while reading the file. It is likely that the file is not a MIDI file or
        /// is severely corrupted.
        ///
        /// This error cannot be ignored, as there is not enough data to continue parsing.
        /// No information about the file could be rescued.
        #[fail(display = "invalid smf file: {}", _0)]
        Invalid(&'static str),

        /// Non-fatal error, but the file is clearly corrupted.
        ///
        /// This kind of error is not emitted by default, only if the `strict` crate feature is
        /// enabled.
        ///
        /// Ignoring these errors can cause whole tracks to be skipped.
        #[fail(display = "malformed smf file: {}", _0)]
        Malformed(&'static str),
    }
    impl ErrorKind {
        /// Get the informative message on what exact part of the SMF format was not respected.
        pub fn message(&self) -> &'static str {
            match *self {
                ErrorKind::Invalid(msg) => msg,
                ErrorKind::Malformed(msg) => msg,
            }
        }
    }

    macro_rules! err_invalid {
        ($msg:expr) => {{
            const ERR_KIND: &'static ErrorKind = &ErrorKind::Invalid($msg);
            ERR_KIND
        }};
    }
    macro_rules! err_malformed {
        ($msg:expr) => {{
            const ERR_KIND: &'static ErrorKind = &ErrorKind::Malformed($msg);
            ERR_KIND
        }};
    }

    pub trait ResultExt<T> {
        fn context(self, ctx: &'static ErrorKind) -> StdResult<T, Error>;
    }
    impl<T> ResultExt<T> for StdResult<T, Error> {
        fn context(self, ctx: &'static ErrorKind) -> StdResult<T, Error> {
            self.map_err(|err| err.chain_ctx(ctx))
        }
    }
    impl<T> ResultExt<T> for StdResult<T, &'static ErrorKind> {
        fn context(self, ctx: &'static ErrorKind) -> StdResult<T, Error> {
            self.map_err(|errkind| Error::from(errkind).chain_ctx(ctx))
        }
    }

    pub type Result<T> = StdResult<T, Error>;
    pub use core::result::Result as StdResult;
}

mod prelude {
    pub(crate) use crate::{
        error::{ErrorKind, Result, ResultExt, StdResult},
        primitive::{u14, u24, u28, u4, u7, IntRead, IntReadBottom7, SplitChecked},
    };
    pub use alloc::vec::Vec;
    pub use core::{convert::TryFrom, marker::PhantomData, mem, ops};
    #[cfg(feature = "std")]
    pub use std::io::{self, Error as IoError, Result as IoResult, Write};

    pub fn bit_range<T>(val: T, range: ops::Range<u32>) -> T
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
mod primitive;
mod riff;
mod smf;

pub use crate::{
    error::{Error, ErrorKind},
    event::{Event, EventKind, MetaMessage, MidiMessage},
    primitive::{Format, Fps, SmpteTime, Timing},
    smf::{Header, Smf, TrackIter, TrackRepr},
};

/// Exotically-sized integers used by the MIDI standard.
pub mod number {
    pub use crate::primitive::{u14, u15, u24, u28, u4, u7};
}

#[cfg(test)]
mod test;
