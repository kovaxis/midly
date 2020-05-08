use crate::prelude::*;

pub type IoResult<W> = StdResult<(), <W as Write>::Error>;

pub trait Write: Send {
    type Error: Send;
    type Seekable: Write<Error = Self::Error, Seekable = Self::Seekable> + Seek;
    fn write_all(&mut self, buf: &[u8]) -> IoResult<Self>;
    fn invalid_input(msg: &'static str) -> Self::Error;
    fn make_seekable(&mut self) -> Option<&mut Self::Seekable> {
        None
    }
}

pub trait Seek: Write {
    fn tell(&mut self) -> StdResult<u64, Self::Error>;
    fn write_at(&mut self, buf: &[u8], pos: u64) -> IoResult<Self>;
}

#[derive(Copy, Clone, Debug)]
enum Never {}

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
    fn never(self) -> ! {
        match self.never {}
    }
}

impl<W: Write> Write for NotSeekable<W> {
    type Error = W::Error;
    type Seekable = Self;
    fn write_all(&mut self, _: &[u8]) -> IoResult<Self> {
        self.never()
    }
    fn invalid_input(msg: &'static str) -> W::Error {
        W::invalid_input(msg)
    }
}

impl<W: Write> Seek for NotSeekable<W> {
    fn tell(&mut self) -> StdResult<u64, W::Error> {
        self.never()
    }
    fn write_at(&mut self, _: &[u8], _: u64) -> IoResult<Self> {
        self.never()
    }
}

#[cfg(feature = "alloc")]
impl Write for Vec<u8> {
    type Error = &'static str;
    type Seekable = Self;
    fn write_all(&mut self, buf: &[u8]) -> IoResult<Self> {
        self.extend_from_slice(buf);
        Ok(())
    }
    fn invalid_input(msg: &'static str) -> &'static str {
        msg
    }
    fn make_seekable(&mut self) -> Option<&mut Self> {
        Some(self)
    }
}
#[cfg(feature = "alloc")]
impl Seek for Vec<u8> {
    fn tell(&mut self) -> StdResult<u64, Self::Error> {
        Ok(self.len() as u64)
    }
    fn write_at(&mut self, buf: &[u8], pos: u64) -> IoResult<Self> {
        let out = self
            .get_mut(pos as usize..pos as usize + buf.len())
            .ok_or("invalid seekback")?;
        out.copy_from_slice(buf);
        Ok(())
    }
}

pub struct Cursor<'a> {
    buf: &'a mut [u8],
    cur: usize,
}
impl<'a> Cursor<'a> {
    pub fn new(slice: &mut [u8]) -> Cursor {
        Cursor { buf: slice, cur: 0 }
    }
    pub fn from_parts(slice: &mut [u8], cursor: usize) -> Cursor {
        assert!(cursor <= slice.len(), "cursor beyond the end of the buffer");
        Cursor {
            buf: slice,
            cur: cursor,
        }
    }
    pub fn into_parts(self) -> (&'a mut [u8], usize) {
        (self.buf, self.cur)
    }
    pub fn slice(&self) -> &[u8] {
        self.buf
    }
    pub fn slice_mut(&mut self) -> &mut [u8] {
        self.buf
    }
    pub fn cursor(&self) -> usize {
        self.cur
    }
}
impl<'a> Write for Cursor<'a> {
    type Error = CursorError;
    type Seekable = Self;
    fn write_all(&mut self, buf: &[u8]) -> IoResult<Self> {
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
    fn invalid_input(msg: &'static str) -> CursorError {
        CursorError::InvalidInput(msg)
    }
    fn make_seekable(&mut self) -> Option<&mut Self> {
        Some(self)
    }
}
impl<'a> Seek for Cursor<'a> {
    fn tell(&mut self) -> StdResult<u64, Self::Error> {
        Ok(self.cur as u64)
    }
    fn write_at(&mut self, buf: &[u8], pos: u64) -> IoResult<Self> {
        let out = self
            .buf
            .get_mut(pos as usize..pos as usize + buf.len())
            .ok_or(CursorError::OutOfSpace)?;
        out.copy_from_slice(buf);
        Ok(())
    }
}

pub enum CursorError {
    OutOfSpace,
    InvalidInput(&'static str),
}
impl<'a> Write for &'a mut [u8] {
    type Error = CursorError;
    type Seekable = NotSeekable<Self>;
    fn write_all(&mut self, buf: &[u8]) -> IoResult<Self> {
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
    fn invalid_input(msg: &'static str) -> CursorError {
        CursorError::InvalidInput(msg)
    }
}

pub struct SeekWrap<T>(pub T);
#[cfg(feature = "std")]
impl<T: io::Write + io::Seek + Send> Write for SeekWrap<T> {
    type Error = io::Error;
    type Seekable = Self;
    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        io::Write::write_all(&mut self.0, buf)
    }
    fn invalid_input(msg: &'static str) -> io::Error {
        io::Error::new(io::ErrorKind::InvalidInput, msg)
    }
    fn make_seekable(&mut self) -> Option<&mut Self> {
        Some(self)
    }
}
#[cfg(feature = "std")]
impl<T: io::Write + io::Seek + Send> Seek for SeekWrap<T> {
    fn tell(&mut self) -> io::Result<u64> {
        io::Seek::seek(&mut self.0, io::SeekFrom::Current(0))
    }
    fn write_at(&mut self, buf: &[u8], pos: u64) -> io::Result<()> {
        io::Seek::seek(&mut self.0, io::SeekFrom::Start(pos))?;
        self.write_all(buf)?;
        io::Seek::seek(&mut self.0, io::SeekFrom::End(0))?;
        Ok(())
    }
}

pub struct NonseekWrap<T>(pub T);
#[cfg(feature = "std")]
impl<T: io::Write + Send> Write for NonseekWrap<T> {
    type Error = io::Error;
    type Seekable = NotSeekable<Self>;
    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        io::Write::write_all(&mut self.0, buf)
    }
    fn invalid_input(msg: &'static str) -> io::Error {
        io::Error::new(io::ErrorKind::InvalidInput, msg)
    }
}

pub(crate) struct WriteCounter(pub u64);
impl Write for WriteCounter {
    type Error = &'static str;
    type Seekable = NotSeekable<Self>;
    fn write_all(&mut self, buf: &[u8]) -> IoResult<Self> {
        self.0 += buf.len() as u64;
        Ok(())
    }
    fn invalid_input(msg: &'static str) -> &'static str {
        msg
    }
}
