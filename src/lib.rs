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
    use failure::{Backtrace, Context, Fail};
    use std::fmt;

    #[derive(Debug)]
    pub struct Error {
        inner: Context<ErrKind>,
    }
    impl Fail for Error {
        fn cause(&self) -> Option<&Fail> {
            self.inner.cause()
        }

        fn backtrace(&self) -> Option<&Backtrace> {
            self.inner.backtrace()
        }
    }
    impl fmt::Display for Error {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            fmt::Display::fmt(&self.inner, f)
        }
    }

    #[derive(Copy, Clone, Debug, PartialEq, Eq, Fail)]
    #[fail(display = "{}", _0)]
    pub enum ErrKind {
        /// A fatal error occurred while parsing the smf file.
        /// This usually means the file is a different type of file than expected (not an SMF).
        #[fail(display = "invalid smf file: {}", _0)]
        Invalid(&'static str),

        /// A non-fatal error occurred while parsing, which usually indicates file corruption.
        ///
        /// This kind of error does not occur with the `lenient` feature enabled.
        /// Note that enabling the `lenient` feature might cause tracks and events to be lost if
        /// they fail to parse.
        #[fail(display = "malformed smf file: {}", _0)]
        Malformed(&'static str),

        /// An non-fatal error which usually does not indicate file corruption, only sloppy
        /// standards-compliance.
        ///
        /// This kind of error is only produced with the `strict` feature enabled.
        #[fail(display = "uncompliant smf file: {}", _0)]
        Pedantic(&'static str),
    }

    pub fn err_invalid(msg: &'static str) -> ErrKind {
        ErrKind::Invalid(msg)
    }
    pub fn err_malformed(msg: &'static str) -> ErrKind {
        ErrKind::Malformed(msg)
    }
    pub fn err_pedantic(msg: &'static str) -> ErrKind {
        ErrKind::Pedantic(msg)
    }

    impl From<Context<ErrKind>> for Error {
        fn from(inner: Context<ErrKind>) -> Error {
            Error { inner }
        }
    }
    impl From<ErrKind> for Error {
        fn from(kind: ErrKind) -> Error {
            Context::new(kind).into()
        }
    }

    pub type Result<T> = StdResult<T, Error>;
    pub use std::result::Result as StdResult;
}

mod prelude {
    pub use crate::{
        error::{err_invalid, err_malformed, err_pedantic, ErrKind, Result, StdResult},
        primitive::{u14, u15, u2, u24, u28, u4, u7, IntRead, IntReadBottom7, SplitChecked},
    };
    pub use bit::BitIndex;
    pub use failure::ResultExt;
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
