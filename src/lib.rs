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
    use failure::{Fail};
    use std::fmt;

    #[cfg(debug_assertions)]
    mod error_impl {
        use failure::{Backtrace, Context, Fail};
        use super::{ErrorKind, Error, ErrorExt};
        
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
            fn cause(&self) -> Option<&Fail> {
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
    
    #[cfg(not(debug_assertions))]
    mod error_impl {
        use failure::{Fail};
        use super::{ErrorKind, ErrorExt, Error};
        
        pub type ErrorInner = ErrorKind;
        impl Fail for Error {}
        impl ErrorExt for Error {
            fn kind(&self) -> ErrorKind {
                self.inner
            }
            fn chain_ctx(self, ctx: ErrorKind) -> Error {
                Error {
                    inner: ctx,
                }
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
    pub use std::result::Result as StdResult;
}

mod prelude {
    pub use crate::{
        error::{err_invalid, err_malformed, err_pedantic, ErrorKind, Result, StdResult, ResultExt},
        primitive::{u14, u15, u2, u24, u28, u4, u7, IntRead, IntReadBottom7, SplitChecked},
    };
    pub use bit::BitIndex;
    pub use std::{marker::PhantomData, mem};
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
    smf::{Header, Smf, SmfBuffer, TrackIter},
};

/// Special-length integers used by the MIDI standard.
pub mod number {
    pub use crate::primitive::{u14, u15, u24, u28, u4, u7};
}

#[cfg(test)]
mod test;
