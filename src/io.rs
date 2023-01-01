//! Provides abstractions over writers, even in `no_std` environments.
//!
//! When the `std` feature is enabled, `IoWrap` and `SeekableWrap` provide a bridge between the
//! `std::io` API and the `midly::io` API.
//! Besides, `write` methods that work with `midly::io::Write` types usually provide a `write_std`
//! variant that works with `std::io::Write` types when the `std` feature is enabled.

use crate::prelude::*;

/// Either `Ok(())` or the error specific to the `W` writer.
pub type WriteResult<W> = StdResult<(), <W as Write>::Error>;

/// A `Write` trait available even in `no_std` environments, and with per-type errors.
pub trait Write {
    /// The error type specific to the writer.
    type Error;
    /// `Self` when the type is seekable, and `NotSeekable<Self>` otherwise.
    type Seekable: Write<Error = Self::Error, Seekable = Self::Seekable> + Seek;

    /// Write a slice of data to the writer.
    ///
    /// Should error if not all of the data could be written.
    fn write(&mut self, buf: &[u8]) -> WriteResult<Self>;
    /// Create an "invalid input"-style error from a string literal.
    fn invalid_input(msg: &'static str) -> Self::Error;
    /// Make this writer seekable, if possible.
    #[inline]
    fn make_seekable(&mut self) -> Option<&mut Self::Seekable> {
        None
    }
}

/// A `Seek` trait available even in `no_std` environments.
///
/// Not all of the `Seek` functionality is required, only the functionality required to write MIDI
/// files.
pub trait Seek: Write {
    /// Where is the writer currently at.
    fn tell(&mut self) -> StdResult<u64, Self::Error>;
    /// Write a slice of data at the given absolute position, and return to the end of the writer
    /// afterwards.
    fn write_at(&mut self, buf: &[u8], pos: u64) -> WriteResult<Self>;
}

#[derive(Copy, Clone, Debug)]
enum Never {}

/// The type used for the [`Seekable`](trait.Write.html#associatedtype.Seekable) associated type on
/// non-seekable writers.
#[derive(Debug)]
pub struct NotSeekable<W> {
    _phantom: PhantomData<W>,
    never: Never,
}
impl<W> Clone for NotSeekable<W> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<W> Copy for NotSeekable<W> {}
impl<W> NotSeekable<W> {
    /// A `NotSeekable` value should never exist.
    /// Because of this, this function never returns, as it could never have been called.
    #[inline]
    pub fn as_never(&self) -> ! {
        match self.never {}
    }
}

impl<W: Write> Write for NotSeekable<W> {
    type Error = W::Error;
    type Seekable = Self;
    #[inline]
    fn write(&mut self, _: &[u8]) -> WriteResult<Self> {
        self.as_never()
    }
    #[inline]
    fn invalid_input(msg: &'static str) -> W::Error {
        W::invalid_input(msg)
    }
}

impl<W: Write> Seek for NotSeekable<W> {
    #[inline]
    fn tell(&mut self) -> StdResult<u64, W::Error> {
        self.as_never()
    }
    #[inline]
    fn write_at(&mut self, _: &[u8], _: u64) -> WriteResult<Self> {
        self.as_never()
    }
}

impl<'a, W: Write> Write for &'a mut W {
    type Error = W::Error;
    type Seekable = W::Seekable;
    #[inline]
    fn write(&mut self, buf: &[u8]) -> WriteResult<W> {
        W::write(self, buf)
    }
    #[inline]
    fn invalid_input(msg: &'static str) -> W::Error {
        W::invalid_input(msg)
    }
    #[inline]
    fn make_seekable(&mut self) -> Option<&mut W::Seekable> {
        W::make_seekable(self)
    }
}

#[cfg(feature = "alloc")]
impl Write for Vec<u8> {
    type Error = &'static str;
    type Seekable = Self;
    #[inline]
    fn write(&mut self, buf: &[u8]) -> WriteResult<Self> {
        self.extend_from_slice(buf);
        Ok(())
    }
    #[inline]
    fn invalid_input(msg: &'static str) -> &'static str {
        msg
    }
    #[inline]
    fn make_seekable(&mut self) -> Option<&mut Self> {
        Some(self)
    }
}
#[cfg(feature = "alloc")]
impl Seek for Vec<u8> {
    #[inline]
    fn tell(&mut self) -> StdResult<u64, Self::Error> {
        Ok(self.len() as u64)
    }
    #[inline]
    fn write_at(&mut self, buf: &[u8], pos: u64) -> WriteResult<Self> {
        let out = self
            .get_mut(pos as usize..pos as usize + buf.len())
            .ok_or("invalid seekback")?;
        out.copy_from_slice(buf);
        Ok(())
    }
}

/// A seekable writer over an in-memory buffer.
///
/// Available even when the `std` and `alloc` features are disabled.
#[derive(Debug)]
pub struct Cursor<'a> {
    buf: &'a mut [u8],
    cur: usize,
}
impl<'a> Cursor<'a> {
    /// Create a new cursor located at the start of the given buffer.
    #[inline]
    pub fn new(buffer: &mut [u8]) -> Cursor {
        Cursor {
            buf: buffer,
            cur: 0,
        }
    }

    /// Create a cursor from a buffer and the cursor within it.
    ///
    /// # Panics
    ///
    /// Panics if `cursor > buffer.len()`.
    #[inline]
    pub fn from_parts(buffer: &mut [u8], cursor: usize) -> Cursor {
        assert!(
            cursor <= buffer.len(),
            "cursor beyond the end of the buffer"
        );
        Cursor {
            buf: buffer,
            cur: cursor,
        }
    }

    /// Yield the underlying buffer and the cursor within it.
    ///
    /// The cursor is guaranteed to be `cursor <= buffer.len()`.
    #[inline]
    pub fn into_parts(self) -> (&'a mut [u8], usize) {
        (self.buf, self.cur)
    }

    /// Get a reference to the whole underlying buffer.
    #[inline]
    pub fn slice(&self) -> &[u8] {
        self.buf
    }

    /// Get a mutable reference to the whole underlying buffer.
    #[inline]
    pub fn slice_mut(&mut self) -> &mut [u8] {
        self.buf
    }

    /// Get the position of the cursor.
    #[inline]
    pub fn cursor(&self) -> usize {
        self.cur
    }

    /// Get a reference to the written portion of the buffer.
    #[inline]
    pub fn written(&self) -> &[u8] {
        &self.buf[..self.cur]
    }

    /// Get a reference to the portion of the buffer that is not yet written.
    #[inline]
    pub fn unwritten(&self) -> &[u8] {
        &self.buf[self.cur..]
    }

    /// Split the buffer into the written and unwritten parts.
    #[inline]
    pub fn split(&self) -> (&[u8], &[u8]) {
        self.buf.split_at(self.cur)
    }

    /// Get a mutable reference to the written portion of the buffer.
    #[inline]
    pub fn written_mut(&mut self) -> &mut [u8] {
        &mut self.buf[..self.cur]
    }

    /// Get a mutable reference to the portion of the buffer that is not yet written.
    #[inline]
    pub fn unwritten_mut(&mut self) -> &mut [u8] {
        &mut self.buf[self.cur..]
    }

    /// Split the buffer into the written and unwritten parts.
    #[inline]
    pub fn split_mut(&mut self) -> (&mut [u8], &mut [u8]) {
        self.buf.split_at_mut(self.cur)
    }
}
impl<'a> Write for Cursor<'a> {
    type Error = CursorError;
    type Seekable = Self;
    #[inline]
    fn write(&mut self, buf: &[u8]) -> WriteResult<Self> {
        //Cannot overflow because `cur <= buf.len()` is always true.
        //Therefore, in order for this to overflow more than the whole address space must be
        //contained within these two slices.
        let up_to = self.cur + buf.len();
        if up_to > self.buf.len() {
            let space = self.buf.len() - self.cur;
            self.buf[self.cur..].copy_from_slice(&buf[..space]);
            self.cur = self.buf.len();
            Err(CursorError::OutOfSpace)
        } else {
            self.buf[self.cur..up_to].copy_from_slice(buf);
            self.cur += buf.len();
            Ok(())
        }
    }
    #[inline]
    fn invalid_input(msg: &'static str) -> CursorError {
        CursorError::InvalidInput(msg)
    }
    #[inline]
    fn make_seekable(&mut self) -> Option<&mut Self> {
        Some(self)
    }
}
impl<'a> Seek for Cursor<'a> {
    #[inline]
    fn tell(&mut self) -> StdResult<u64, Self::Error> {
        Ok(self.cur as u64)
    }
    #[inline]
    fn write_at(&mut self, buf: &[u8], pos: u64) -> WriteResult<Self> {
        let out = self
            .buf
            .get_mut(pos as usize..pos as usize + buf.len())
            .ok_or(CursorError::OutOfSpace)?;
        out.copy_from_slice(buf);
        Ok(())
    }
}

/// The errors that can arise when writing to an in-memory buffer.
#[derive(Debug, Clone)]
pub enum CursorError {
    /// The in-memory buffer was too small.
    OutOfSpace,
    /// The input SMF was invalid.
    InvalidInput(&'static str),
}
impl<'a> Write for &'a mut [u8] {
    type Error = CursorError;
    type Seekable = NotSeekable<Self>;
    #[inline]
    fn write(&mut self, buf: &[u8]) -> WriteResult<Self> {
        if buf.len() > self.len() {
            self.copy_from_slice(&buf[..self.len()]);
            *self = &mut [];
            Err(CursorError::OutOfSpace)
        } else {
            self[..buf.len()].copy_from_slice(buf);
            let slice = mem::replace(self, &mut []);
            *self = &mut slice[buf.len()..];
            Ok(())
        }
    }
    #[inline]
    fn invalid_input(msg: &'static str) -> CursorError {
        CursorError::InvalidInput(msg)
    }
}

/// Bridge between a `midly::io::Write` type and a `std::io::Write` type.
///
/// Always available, but only implements `midly::io::Write` when the `std` feature is enabled.
#[derive(Debug, Clone, Default)]
pub struct IoWrap<T>(pub T);
#[cfg(feature = "std")]
impl<T: io::Write> Write for IoWrap<T> {
    type Error = io::Error;
    type Seekable = NotSeekable<Self>;
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<()> {
        io::Write::write_all(&mut self.0, buf)
    }
    #[inline]
    fn invalid_input(msg: &'static str) -> io::Error {
        io::Error::new(io::ErrorKind::InvalidInput, msg)
    }
}

/// Bridge between a `midly::io::{Write, Seek}` type and a `std::io::{Write, Seek}` type.
///
/// Always available, but only implements `midly::io::{Write, Seek}` when the `std` feature is
/// enabled.
#[derive(Debug, Clone, Default)]
pub struct SeekableWrap<T>(pub T);
#[cfg(feature = "std")]
impl<T: io::Write + io::Seek> Write for SeekableWrap<T> {
    type Error = io::Error;
    type Seekable = Self;
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<()> {
        io::Write::write_all(&mut self.0, buf)
    }
    #[inline]
    fn invalid_input(msg: &'static str) -> io::Error {
        io::Error::new(io::ErrorKind::InvalidInput, msg)
    }
    #[inline]
    fn make_seekable(&mut self) -> Option<&mut Self> {
        Some(self)
    }
}
#[cfg(feature = "std")]
impl<T: io::Write + io::Seek> Seek for SeekableWrap<T> {
    #[inline]
    fn tell(&mut self) -> io::Result<u64> {
        io::Seek::seek(&mut self.0, io::SeekFrom::Current(0))
    }
    #[inline]
    fn write_at(&mut self, buf: &[u8], pos: u64) -> io::Result<()> {
        io::Seek::seek(&mut self.0, io::SeekFrom::Start(pos))?;
        self.write(buf)?;
        io::Seek::seek(&mut self.0, io::SeekFrom::End(0))?;
        Ok(())
    }
}

/// Counts the amount of bytes written to it, but otherwise ignores the actual bytes written.
pub(crate) struct WriteCounter(pub u64);
impl Write for WriteCounter {
    type Error = &'static str;
    type Seekable = NotSeekable<Self>;
    #[inline]
    fn write(&mut self, buf: &[u8]) -> WriteResult<Self> {
        self.0 += buf.len() as u64;
        Ok(())
    }
    #[inline]
    fn invalid_input(msg: &'static str) -> &'static str {
        msg
    }
}
