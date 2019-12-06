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
//! //Parse bytes in a separate step
//! let smf = Smf::parse(&bytes).unwrap();
//! ```
//!
//! # About features
//!
//! The mode in which the crate works is configurable through the use of cargo features.
//! Three optional features are available: `std`, `lenient` and `strict`.
//! Of these only `std` is enabled by default.
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
//! - The `lenient` feature
//!
//!   By default `midly` will reject obviously corrupted files, throwing an error with kind
//!   `ErrorKind::Malformed`.
//!   With the `lenient` feature enabled the parser will do a "best-effort" attempt at parsing the
//!   file, and will suppress any of these errors.
//!   Note that even though the file might parse successfully, entire tracks might be lost.
//!
//! - The `strict` feature
//!
//!   By default `midly` gives some leeway for uncompliant MIDI files, through the
//!   `MetaMessage::Unknown` event variant.
//!   Enabling the `strict` feature will promote these malformed events to an error of kind
//!   `ErrorKind::Pedantic` instead.
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
//! This means that raw, non-SMF, MIDI streams exist, for example those generating by MIDI
//! keyboards.
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
            fn chain_ctx(self, ctx: ErrorKind) -> Error {
                Error {
                    inner: Fail::context(self, ctx),
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
        impl From<ErrorKind> for Error {
            fn from(kind: ErrorKind) -> Error {
                Error {
                    inner: Context::new(kind),
                }
            }
        }
    }

    #[cfg(not(all(debug_assertions, feature = "std")))]
    mod error_impl {
        use super::{Error, ErrorExt, ErrorKind};
        use failure::Fail;

        pub type ErrorInner = ErrorKind;
        impl Fail for Error {}
        impl ErrorExt for Error {
            fn kind(&self) -> ErrorKind {
                self.inner
            }
            fn chain_ctx(self, ctx: ErrorKind) -> Error {
                Error { inner: ctx }
            }
        }
        impl From<ErrorKind> for Error {
            fn from(inner: ErrorKind) -> Error {
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
        fn chain_ctx(self, ctx: ErrorKind) -> Error;
    }

    /// The type of error that occurred while parsing.
    ///
    /// As a library consumer, detailed errors about what specific part of the MIDI spec was
    /// violated are not very useful.
    /// For this reason, errors are broadly categorized into 3 classes, and specific error info is
    /// provided as a non-normative string literal.
    #[derive(Copy, Clone, Debug, Fail)]
    #[fail(display = "{}", _0)]
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
        /// This kind of error is emitted by default, but by enabling the `lenient` crate feature
        /// they can be ignored.
        ///
        /// Ignoring these errors can cause whole tracks to be skipped.
        #[fail(display = "malformed smf file: {}", _0)]
        Malformed(&'static str),

        /// Subtle violations to the MIDI spec which occur in the wild, especially in
        /// regard to midi meta-messages.
        ///
        /// This kind of error is ignored by default, but enabling the `strict` feature will
        /// force the parser to emit them.
        ///
        /// Ignoring these errors can cause isolated events to be skipped and reported as
        /// `MetaMessage::Unknown`.
        #[fail(display = "uncompliant smf file: {}", _0)]
        Pedantic(&'static str),
    }
    impl ErrorKind {
        /// Get the informative message on what exact part of the SMF format was not respected.
        pub fn message(&self) -> &'static str {
            match *self {
                ErrorKind::Invalid(msg) => msg,
                ErrorKind::Malformed(msg) => msg,
                ErrorKind::Pedantic(msg) => msg,
            }
        }
    }

    pub fn err_invalid(msg: &'static str) -> ErrorKind {
        ErrorKind::Invalid(msg)
    }
    pub fn err_malformed(msg: &'static str) -> ErrorKind {
        ErrorKind::Malformed(msg)
    }
    pub fn err_pedantic(msg: &'static str) -> ErrorKind {
        ErrorKind::Pedantic(msg)
    }

    pub trait ResultExt<T> {
        fn context(self, ctx: ErrorKind) -> StdResult<T, Error>;
    }
    impl<T> ResultExt<T> for StdResult<T, Error> {
        fn context(self, ctx: ErrorKind) -> StdResult<T, Error> {
            self.map_err(|err| err.chain_ctx(ctx))
        }
    }
    impl<T> ResultExt<T> for StdResult<T, ErrorKind> {
        fn context(self, ctx: ErrorKind) -> StdResult<T, Error> {
            self.map_err(|errkind| Error::from(errkind).chain_ctx(ctx))
        }
    }

    pub type Result<T> = StdResult<T, Error>;
    pub use core::result::Result as StdResult;
}

mod prelude {
    pub(crate) use crate::{
        error::{
            err_invalid, err_malformed, err_pedantic, ErrorKind, Result, ResultExt, StdResult,
        },
        primitive::{u14, u24, u28, u4, u7, IntRead, IntReadBottom7, SplitChecked},
    };
    pub use alloc::vec::Vec;
    pub use core::{marker::PhantomData, mem, ops, convert::TryFrom};
    #[cfg(feature = "std")]
    pub use std::io::{self, Write, Error as IoError, Result as IoResult};

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

/// All sort of events and their parsing.
mod event;

/// Simple building-block data that can be read in one go.
/// All are stored in a fixed size (`Sized`) representation.
/// Also, primitives advance the file pointer when read.
mod primitive;

/// Parsing for SMF files, chunks and tracks.
mod smf;

pub use crate::{
    error::{Error, ErrorKind},
    event::{Event, EventKind, MetaMessage, MidiMessage},
    primitive::{Format, Fps, SmpteTime, Timing},
    smf::{Header, Smf, TrackIter, TrackRepr},
};

/// Special-length integers used by the MIDI standard.
pub mod number {
    pub use crate::primitive::{u14, u15, u24, u28, u4, u7};
}

#[cfg(test)]
mod test;
