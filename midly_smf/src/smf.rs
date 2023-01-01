//! Specific to the SMF packaging of MIDI streams.

use midly_core::Fps;

#[cfg(feature = "alloc")]
use crate::playback::{
    OrderedIntoIter, OrderedIter, OrderedIterMut, TimedIntoIter, TimedIter, TimedIterMut,
};
use crate::{event::TrackEvent, prelude::*, riff};

/// How many events per byte to estimate when allocating memory for events while parsing.
///
/// A value that is too large (ie. too few bytes/event), will overallocate, while a value that is
/// too small (ie. too many bytes/event) will underallocate.
///
/// Usually, since memory is cheap it's better to overallocate, since reallocating the buffer may
/// result in costly memory moves.
/// This means that it's better to err on the large side (too few bytes/event).
///
/// Real-world tests show that without running status, the average is a little above 4 bytes/event,
/// and with running status enabled it's a little above 3 bytes/event.
/// This makes sense, since it's DeltaTime [+ Status] + Key + Velocity for NoteOn and NoteOff
/// events, which should make up the bulk of most MIDI files.
///
/// Erring on the large side for events/byte (erring on the small side for bytes/event), we can
/// approximate to 3 bytes/event.
#[cfg(feature = "alloc")]
const BYTES_TO_EVENTS: f32 = 1.0 / 3.0;

/// How many bytes per event to estimate when allocating memory when writing.
///
/// A value that is too large will overallocate space for bytes, while a value that's too small
/// will underallocate bytes.
///
/// Since the writer uses running status by default, a value a bit over `3` will allocate enough for
/// almost all cases (Except for eg. info tracks, which usually have a high byte/event count
/// because they contain text. However, these tracks are small enough that reallocating doesn't
/// matter too much).
#[cfg(feature = "alloc")]
const EVENTS_TO_BYTES: f32 = 3.4;

/// How many bytes must a MIDI body have in order to enable multithreading.
///
/// When writing, the MIDI body size is estimated from the event count using `BYTES_PER_EVENT`.
#[cfg(feature = "parallel")]
const PARALLEL_ENABLE_THRESHOLD: usize = 3 * 1024;

/// A single track: simply a list of track events.
///
/// Only available with the `alloc` feature enabled.
#[cfg(feature = "alloc")]
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Track<'a> {
    pub events: Vec<TrackEvent<'a>>,
}
#[cfg(feature = "alloc")]
impl<'a> Track<'a> {
    #[inline]
    pub fn new(events: Vec<TrackEvent<'a>>) -> Track<'a> {
        Track { events }
    }

    #[inline]
    pub fn make_owned(&mut self) {
        for ev in self.events.iter_mut() {
            ev.make_owned();
        }
    }

    #[inline]
    pub fn into_owned(mut self) -> Track<'static> {
        unsafe {
            self.make_owned();
            mem::transmute::<Track<'a>, Track<'static>>(self)
        }
    }

    #[inline]
    pub fn make_static(&mut self) {
        for ev in self.events.iter_mut() {
            ev.make_static();
        }
    }

    #[inline]
    pub fn into_static(mut self) -> Track<'static> {
        unsafe {
            self.make_static();
            mem::transmute::<Track<'a>, Track<'static>>(self)
        }
    }

    #[inline]
    pub fn forget_drop(mut self) {
        unsafe {
            self.events.set_len(0);
        }
    }
}
#[cfg(feature = "alloc")]
impl<'a> IntoIterator for Track<'a> {
    type IntoIter = alloc::vec::IntoIter<TrackEvent<'a>>;
    type Item = TrackEvent<'a>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.events.into_iter()
    }
}
#[cfg(feature = "alloc")]
impl<'a> IntoIterator for &'a Track<'a> {
    type IntoIter = core::slice::Iter<'a, TrackEvent<'a>>;
    type Item = &'a TrackEvent<'a>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.events.iter()
    }
}
#[cfg(feature = "alloc")]
impl<'a> IntoIterator for &'a mut Track<'a> {
    type IntoIter = core::slice::IterMut<'a, TrackEvent<'a>>;
    type Item = &'a mut TrackEvent<'a>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.events.iter_mut()
    }
}

/// Represents a single `.mid` Standard Midi File.
/// If you're casually looking to parse a `.mid` file, this is the type you're looking for.
///
/// This type is only available with the `alloc` feature enabled.
/// If you're looking for a fully `no_std` alternative, see the [`parse`](fn.parse.html) function.
#[cfg(feature = "alloc")]
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Smf<'a> {
    /// The header of this MIDI file, indicating tempo information and track format.
    pub header: Header,
    /// A list of tracks within this MIDI file.
    pub tracks: Vec<Track<'a>>,
}
#[cfg(feature = "alloc")]
impl<'a> Smf<'a> {
    /// Create a new empty `Smf` with zero tracks, using the given header.
    #[inline]
    pub fn new(header: Header) -> Smf<'a> {
        Smf {
            header,
            tracks: vec![],
        }
    }

    /// Parse a `.mid` Standard Midi File from its raw bytes.
    /// If you casually want to parse `.mid` files, this is the function you're looking for.
    pub fn parse(raw: &mut [u8]) -> Result<Smf> {
        let (header, tracks) = parse(raw)?;
        let track_count_hint = tracks.track_count_hint;
        let tracks = tracks.collect_tracks()?;
        validate_smf(&header, track_count_hint, tracks.len())?;
        Ok(Smf { header, tracks })
    }

    /// Encodes and writes the file to the given generic writer.
    ///
    /// Note that this function requires a `midly::io::Write` writer, not a `std::io::Write` writer.
    /// This makes it possible to support `no_std` environments, as well as custom writer errors.
    /// If you're looking to write to a `File`, see the [`save`](#method.save) method.
    /// If you're looking to write to a `std::io::Write` writer, see the
    /// [`write_std`](#method.write_std) method.
    ///
    /// This function is always available, even in `no_std` environments.
    #[inline]
    pub fn write<W: Write>(&self, out: &mut W) -> WriteResult<W> {
        write(&self.header, &self.tracks, out)
    }

    /// Encodes and writes the file to the given `std::io::Write` writer.
    ///
    /// This function is similar to the [`write`](#method.write) method, but writes to a
    /// `std::io::Write` writer instead of a `midly::io::Write` writer.
    ///
    /// This function is only available with the `std` feature enabled.
    #[cfg(feature = "std")]
    #[inline]
    pub fn write_std<W: io::Write>(&self, out: W) -> io::Result<()> {
        write_std(&self.header, &self.tracks, out)
    }

    /// Encodes and writes the file to the given path.
    ///
    /// This function is only available with the `std` feature enabled.
    #[cfg(feature = "std")]
    #[inline]
    pub fn save<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        /// A non-generic, non-inline function.
        /// This means that this function will be compiled and monomorphized once, and reused for
        /// every call to `save`.
        fn save_impl(smf: &Smf, path: &Path) -> io::Result<()> {
            smf.write(&mut crate::io::IoWrap(File::create(path)?))
        }
        save_impl(self, path.as_ref())
    }

    #[inline]
    pub fn make_owned(&mut self) {
        for track in self.tracks.iter_mut() {
            track.make_owned();
        }
    }

    #[inline]
    pub fn into_owned(mut self) -> Smf<'static> {
        unsafe {
            self.make_owned();
            mem::transmute::<Smf<'a>, Smf<'static>>(self)
        }
    }

    /// Remove any lifetimed data from this event to create an `Smf` with `'static`
    /// lifetime that can be stored and moved everywhere, solving borrow checker issues.
    ///
    /// This method creates a copy of the `Smf` structure. See the `make_static` method for an
    /// in-place solution.
    ///
    /// WARNING: Any bytestrings, including meta messages, SysEx dumps and escape sequences will be
    /// replaced by empty bytestrings.
    #[inline]
    pub fn to_static(&self) -> Smf<'static> {
        self.clone().into_static()
    }

    /// Remove any lifetimed data from this event to create an `Smf` with `'static`
    /// lifetime that can be stored and moved everywhere, solving borrow checker issues.
    ///
    /// This method consumes the `Smf` structure, reusing the backing memory.
    ///
    /// WARNING: Any bytestrings, including meta messages, SysEx dumps and escape sequences will be
    /// replaced by empty bytestrings.
    #[inline]
    pub fn into_static(mut self) -> Smf<'static> {
        for track in self.tracks.iter_mut() {
            track.make_static();
        }
        unsafe { mem::transmute::<Smf<'a>, Smf<'static>>(self) }
    }

    /// Iterates over the events in the different tracks, in order.
    #[inline]
    pub fn ordered_iter(&self) -> OrderedIter {
        OrderedIter::new(self)
    }

    /// Iterates over the events in the different tracks, in order.
    #[inline]
    pub fn ordered_iter_mut(&'a mut self) -> OrderedIterMut<'a> {
        OrderedIterMut::new(self)
    }

    /// Iterates over the events in the different tracks, in order.
    #[inline]
    pub fn into_ordered_iter(self) -> OrderedIntoIter<'a> {
        OrderedIntoIter::new(self)
    }

    /// Iterates over the events in the different tracks, in order.
    #[inline]
    pub fn timed_iter(&self) -> TimedIter {
        TimedIter::new(self)
    }

    /// Iterates over the events in the different tracks, in order.
    #[inline]
    pub fn timed_iter_mut(&'a mut self) -> TimedIterMut<'a> {
        TimedIterMut::new(self)
    }

    /// Iterates over the events in the different tracks, in order.
    #[inline]
    pub fn into_timed_iter(self) -> TimedIntoIter<'a> {
        TimedIntoIter::new(self)
    }

    #[inline]
    pub fn forget_drop(self) {
        for track in self.tracks {
            track.forget_drop();
        }
    }
}

#[cfg(feature = "alloc")]
fn validate_smf(header: &Header, track_count_hint: u16, track_count: usize) -> Result<()> {
    if cfg!(feature = "strict") {
        ensure!(
            track_count_hint as usize == track_count,
            err_malformed!("file has a different amount of tracks than declared")
        );
        ensure!(
            header.format != Format::SingleTrack || track_count == 1,
            err_malformed!("singletrack format file has multiple tracks")
        );
    }
    Ok(())
}

/// Parse a raw MIDI file lazily, yielding its header and a lazy track iterator.
/// No allocations are made.
///
/// The track iterator that is returned yields event iterators, which in turn yield concrete events.
///
/// This function is always available, even in `no_std` environments.
pub fn parse(raw: &mut [u8]) -> Result<(Header, TrackIter)> {
    let raw = match raw.get(..4) {
        Some(b"RIFF") => riff::unwrap(raw)?,
        Some(b"MThd") => raw,
        _ => bail!(err_invalid!("not a midi file")),
    };
    let mut chunks = ChunkIter::new(raw);
    let (header, track_count) = match chunks.next() {
        Some(maybe_chunk) => match maybe_chunk.context(err_invalid!("invalid midi header"))? {
            Chunk::Header(header, track_count) => Ok((header, track_count)),
            Chunk::Track(_) => Err(err_invalid!("expected header, found track")),
        },
        None => Err(err_invalid!("no midi header chunk")),
    }?;
    let tracks = chunks.as_tracks(track_count);
    Ok((header, tracks))
}

/// Encode and write a generic MIDI file into the given generic writer.
/// The MIDI file is represented by a header and a list of tracks.
///
/// # Errors
///
/// The MIDI writer raises almost no errors by itself, it only bubbles errors from the underlying
/// writer.
/// The only exception to this rule are extreme cases that break the limits of the MIDI spec: if
/// there are more than 65535 tracks, if the data for a single event is 256MB or larger, or if the
/// total size of any track is 4GB or larger.
///
/// # Implementation notes
///
/// Currently this function will attempt to use multiple threads to encode the file if possible and
/// the file is large enough to make it worth it.
///
/// Otherwise, each track will be written to an in-memory buffer before writing to disk.
///
/// If allocation is disabled, but the writer is seekable, the file will be written once and it
/// will be seeked back in order to write down the chunk sizes.
///
/// Otherwise, encoding will happen twice: once to determine the size of the chunks and once again
/// to actually write down the file.
pub fn write<'a, T, E, W>(header: &Header, tracks: T, out: &mut W) -> WriteResult<W>
where
    T: IntoIterator<Item = E>,
    T::IntoIter: ExactSizeIterator + Clone + Send,
    E: IntoIterator<Item = &'a TrackEvent<'a>>,
    E::IntoIter: Clone + Send,
    W: Write,
{
    let tracks = tracks.into_iter().map(|events| events.into_iter());
    //Write the header first
    Chunk::write_header(header, tracks.len(), out)?;

    //Try to write the file in parallel
    #[cfg(feature = "parallel")]
    {
        //Figure out whether multithreading is worth it
        let event_count = tracks
            .clone()
            .map(|track| track.into_iter().size_hint().0)
            .sum::<usize>();
        if (event_count as f32 * EVENTS_TO_BYTES) > PARALLEL_ENABLE_THRESHOLD as f32 {
            use rayon::prelude::*;

            //Write out the tracks in parallel into several different buffers
            let mut track_chunks = Vec::new();
            tracks
                .collect::<Vec<_>>()
                .into_par_iter()
                .map(|track| {
                    let mut track_chunk = Vec::new();
                    Chunk::write_to_vec(track, &mut track_chunk)?;
                    Ok(track_chunk)
                })
                .collect_into_vec(&mut track_chunks);

            //Write down the tracks sequentially and in order
            for result in track_chunks {
                let track_chunk = result.map_err(W::invalid_input)?;
                out.write(&track_chunk)?;
            }
            return Ok(());
        }
    }

    #[cfg(feature = "alloc")]
    {
        //Write the tracks into a buffer before writing out to the file
        let mut buf = Vec::new();
        for track in tracks {
            Chunk::write_to_vec(track, &mut buf).map_err(|msg| W::invalid_input(msg))?;
            out.write(&buf)?;
        }
        return Ok(());
    }

    #[allow(unreachable_code)]
    {
        if let Some(out) = out.make_seekable() {
            //Write down using seeks if the writer is seekable
            for track in tracks {
                Chunk::write_seek(track, out)?;
            }
            return Ok(());
        }

        //Last resort: do probe-writing.
        //Two passes are done: one to find out the size of the chunk and another to actually
        //write the chunk.
        for track in tracks {
            Chunk::write_probe(track, out)?;
        }
        Ok(())
    }
}

/// Similar to [`write`](fn.write.html), but writes to a `std::io::Write` writer instead of a
/// `midly::io::Write` writer.
///
/// This function is only available with the `std` feature enabled.
#[cfg(feature = "std")]
#[inline]
pub fn write_std<'a, T, E, W>(header: &Header, tracks: T, out: W) -> io::Result<()>
where
    T: IntoIterator<Item = E>,
    T::IntoIter: ExactSizeIterator + Clone + Send,
    E: IntoIterator<Item = &'a TrackEvent<'a>>,
    E::IntoIter: Clone + Send,
    W: io::Write,
{
    write(header, tracks, &mut crate::io::IoWrap(out))
}

#[derive(Debug)]
struct ChunkIter<'a> {
    /// Starts at the current index, ends at EOF.
    raw: &'a mut [u8],
}
impl<'a> ChunkIter<'a> {
    fn new(raw: &'a mut [u8]) -> ChunkIter {
        ChunkIter { raw }
    }

    fn as_tracks(self, track_count_hint: u16) -> TrackIter<'a> {
        TrackIter {
            chunks: self,
            track_count_hint,
        }
    }
}
impl<'a> Iterator for ChunkIter<'a> {
    type Item = Result<Chunk<'a>>;
    fn next(&mut self) -> Option<Result<Chunk<'a>>> {
        //Flip around option and result
        match Chunk::read(&mut self.raw) {
            Ok(Some(chunk)) => Some(Ok(chunk)),
            Ok(None) => None,
            Err(err) => {
                //Ensure `Chunk::read` isn't called again, by setting read pointer to EOF (len 0)
                //This is to prevent use of corrupted state (such as reading a new Chunk from the
                //middle of a malformed message)
                self.raw = &mut [];
                Some(Err(err))
            }
        }
    }
}

#[derive(Debug)]
enum Chunk<'a> {
    Header(Header, u16),
    Track(&'a mut [u8]),
}
impl<'a> Chunk<'a> {
    /// Should be called with a byte slice at least as large as the chunk (ideally until EOF).
    /// The slice will be modified to point to the next chunk.
    /// If we're *exactly* at EOF (slice length 0), returns a None signalling no more chunks.
    fn read(raw: &mut &'a mut [u8]) -> Result<Option<Chunk<'a>>> {
        Ok(loop {
            if raw.is_empty() {
                break None;
            }
            let id = read_slice(raw, 4).context(err_invalid!("failed to read chunkid"))?;
            let len = read_u32(raw).context(err_invalid!("failed to read chunklen"))?;
            let chunkdata =
                match read_slice(raw, len as usize).context(err_malformed!("truncated chunk")) {
                    Ok(chunkdata) => chunkdata,
                    Err(e) => {
                        if cfg!(feature = "strict") {
                            bail!(e)
                        } else {
                            //Just use the remainder of the file
                            mem::replace(raw, &mut [])
                        }
                    }
                };
            match &id[..] {
                b"MThd" => {
                    let (header, track_count) = Header::read(chunkdata)?;
                    break Some(Chunk::Header(header, track_count));
                }
                b"MTrk" => {
                    break Some(Chunk::Track(chunkdata));
                }
                //Unknown chunk, just ignore and read the next one
                _ => {}
            }
        })
    }

    /// Write a header chunk into a writer.
    fn write_header<W: Write>(header: &Header, track_count: usize, out: &mut W) -> WriteResult<W> {
        let mut header_chunk = [0; 14];
        let track_count = u16::try_from(track_count)
            .map_err(|_| W::invalid_input("track count exceeds 16 bit range"))?;
        let header = header.encode(track_count);
        header_chunk[0..4].copy_from_slice(&b"MThd"[..]);
        header_chunk[4..8].copy_from_slice(&(header.len() as u32).to_be_bytes()[..]);
        header_chunk[8..14].copy_from_slice(&header[..]);
        out.write(&header_chunk[..])?;
        Ok(())
    }

    /// Write a single track chunk using the probe method.
    ///
    /// When probing, the chunk is written twice: one to find out the length of the chunk and again
    /// to actually write the chunk contents.
    fn write_probe<W: Write>(
        track: impl Iterator<Item = &'a TrackEvent<'a>> + Clone,
        out: &mut W,
    ) -> WriteResult<W> {
        let mut counter = WriteCounter(0);
        Self::write_raw(track.clone(), &mut counter).map_err(W::invalid_input)?;
        let len = Self::check_len::<W, _>(counter.0)?;
        let mut head = [b'M', b'T', b'r', b'k', 0, 0, 0, 0];
        head[4..8].copy_from_slice(&len);
        out.write(&head)?;
        Self::write_raw(track, out)?;
        Ok(())
    }

    /// Write a single chunk using the seek method.
    ///
    /// The chunk is written once, then the writer is seeked back and the chunk length is written
    /// last.
    fn write_seek<W: Write + Seek>(
        track: impl Iterator<Item = &'a TrackEvent<'a>>,
        out: &mut W,
    ) -> WriteResult<W> {
        out.write(b"MTrk\0\0\0\0")?;
        let start = out.tell()?;
        Self::write_raw(track, out)?;
        let len = Self::check_len::<W, _>(out.tell()? - start)?;
        out.write_at(&len, start - 4)?;
        Ok(())
    }

    /// Write a chunk to an in-memory `Vec`.
    ///
    /// Because the output is in-memory, the chunk can simply wind back and write the chunk length
    /// last.
    #[cfg(feature = "alloc")]
    fn write_to_vec(
        track: impl Iterator<Item = &'a TrackEvent<'a>>,
        out: &mut Vec<u8>,
    ) -> WriteResult<Vec<u8>> {
        let cap = (track.size_hint().0 as f32 * EVENTS_TO_BYTES) as usize;
        out.clear();
        out.reserve(8 + cap);
        out.extend_from_slice(b"MTrk\0\0\0\0");
        Self::write_raw(track, out)?;
        let len = Self::check_len::<Vec<u8>, _>(out.len() - 8)?;
        out[4..8].copy_from_slice(&len);
        Ok(())
    }

    /// Auxiliary method. Iterate over the events of a track and write them out.
    fn write_raw<W: Write>(
        track: impl Iterator<Item = &'a TrackEvent<'a>>,
        out: &mut W,
    ) -> WriteResult<W> {
        let mut running_status = None;
        for ev in track {
            ev.write(&mut running_status, out)?;
        }
        Ok(())
    }

    /// Auxiliary method. Given an arbitrary-width length, fit it into a 32-bit big-endian integer,
    /// reporting an error if it does not fit.
    fn check_len<W, T>(len: T) -> StdResult<[u8; 4], W::Error>
    where
        u32: TryFrom<T>,
        W: Write,
    {
        let len = u32::try_from(len)
            .map_err(|_| W::invalid_input("midi chunk size exceeds 32 bit range"))?;
        Ok(len.to_be_bytes())
    }
}

/// A MIDI file header, indicating metadata about the file.
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct Header {
    /// Information about how should the tracks be laid out when playing them back.
    pub format: Format,
    /// Tempo information about the file.
    ///
    /// Usually it's not possible to determine the timing of a file with just this field, the first
    /// few events of the first track must be parsed in the best case, and in the worst case the
    /// file might have changing tempos along the song.
    pub timing: Timing,
}
impl Header {
    /// Create a new header from its raw parts.
    #[inline]
    pub fn new(format: Format, timing: Timing) -> Header {
        Header { format, timing }
    }

    /// Read the contents of a header chunk, including the `Header` and the track count.
    fn read(mut raw: &mut [u8]) -> Result<(Header, u16)> {
        let format = Format::from_bits(read_u16(&mut raw)?)?;
        let track_count = read_u16(&mut raw)?;
        let timing = Timing::read(&mut raw)?;
        Ok((Header::new(format, timing), track_count))
    }

    #[inline]
    fn encode(&self, track_count: u16) -> [u8; 6] {
        let mut bytes = [0; 6];
        bytes[0..2].copy_from_slice(&self.format.as_bits().to_be_bytes()[..]);
        bytes[2..4].copy_from_slice(&track_count.to_be_bytes()[..]);
        bytes[4..6].copy_from_slice(&self.timing.encode()[..]);
        bytes
    }
}

/// An iterator over all *tracks* in a Standard Midi File.
/// Created by the [`parse`](fn.parse.html) function.
///
/// This type is always available, even in `no_std` environments.
#[derive(Debug)]
pub struct TrackIter<'a> {
    chunks: ChunkIter<'a>,
    track_count_hint: u16,
}
impl<'a> TrackIter<'a> {
    /// Create an event iterator from raw SMF bytes, excluding the header.
    ///
    /// The main way to obtain raw SMF without a header is the [`unread`](#method.unread) method.
    #[inline]
    pub fn new(raw: &mut [u8]) -> TrackIter {
        TrackIter {
            chunks: ChunkIter::new(raw),
            track_count_hint: 0,
        }
    }

    /// Peek at the remaining unparsed bytes in the file.
    #[inline]
    pub fn unread(&self) -> &[u8] {
        self.chunks.raw
    }

    /// Peek at the remaining unparsed bytes in the file.
    #[inline]
    pub fn unread_mut(&mut self) -> &mut [u8] {
        self.chunks.raw
    }

    /// Parse and collect the remaining unparsed tracks into a `Vec` of tracks.
    ///
    /// This function is only available with the `alloc` feature enabled.
    #[cfg(feature = "alloc")]
    pub fn collect_tracks(self) -> Result<Vec<Track<'a>>> {
        //Attempt to use multiple threads if possible and advantageous
        #[cfg(feature = "parallel")]
        {
            if self.unread().len() >= PARALLEL_ENABLE_THRESHOLD {
                use rayon::prelude::*;

                let chunk_vec = self.collect::<Result<Vec<_>>>()?;
                return chunk_vec
                    .into_par_iter()
                    .map(EventIter::into_track)
                    .collect::<Result<Vec<Track>>>();
            }
        }
        //Fall back to single-threaded
        self.map(|r| r.and_then(EventIter::into_track))
            .collect::<Result<Vec<Track>>>()
    }
}
impl<'a> Iterator for TrackIter<'a> {
    type Item = Result<EventIter<'a>>;

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (
            self.track_count_hint as usize,
            Some(self.track_count_hint as usize),
        )
    }

    #[inline]
    fn next(&mut self) -> Option<Result<EventIter<'a>>> {
        loop {
            if let Some(chunk) = self.chunks.next() {
                self.track_count_hint = self.track_count_hint.saturating_sub(1);
                match chunk {
                    Ok(Chunk::Track(track)) => break Some(Ok(EventIter::new(track))),
                    //Read another header (?)
                    Ok(Chunk::Header(..)) => {
                        if cfg!(feature = "strict") {
                            break Some(Err(err_malformed!("found duplicate header").into()));
                        } else {
                            //Ignore duplicate header
                        }
                    }
                    //Failed to read chunk
                    Err(err) => {
                        if cfg!(feature = "strict") {
                            break Some(Err(err).context(err_malformed!("invalid chunk")));
                        } else {
                            //Ignore invalid chunk
                        }
                    }
                }
            } else {
                break None;
            }
        }
    }
}

/// An iterator over the events of a single track.
/// Yielded by the [`TrackIter`](struct.TrackIter.html) iterator.
///
/// This iterator is lazy, it parses events as it goes, and therefore produces `Result<TrackEvent>>`
/// rather than `TrackEvent`.
///
/// This type is always available, even in `no_std` environments.
#[derive(Debug)]
pub struct EventIter<'a> {
    raw: &'a mut [u8],
    running_status: Option<u8>,
}
impl<'a> EventIter<'a> {
    #[inline]
    pub fn new(raw: &mut [u8]) -> EventIter {
        EventIter {
            raw,
            running_status: None,
        }
    }

    /// Get the remaining unread bytes.
    #[inline]
    pub fn unread(&self) -> &[u8] {
        self.raw
    }

    /// Get the remaining unread bytes.
    #[inline]
    pub fn unread_mut(&mut self) -> &mut [u8] {
        self.raw
    }

    /// Get the current running status of the track.
    #[inline]
    pub fn running_status(&self) -> Option<u8> {
        self.running_status
    }

    /// Modify the current running status of the track.
    #[inline]
    pub fn running_status_mut(&mut self) -> &mut Option<u8> {
        &mut self.running_status
    }

    #[cfg(feature = "alloc")]
    fn estimate_events(&self) -> usize {
        (self.raw.len() as f32 * BYTES_TO_EVENTS) as usize
    }

    #[cfg(feature = "alloc")]
    pub fn into_track(mut self) -> Result<Track<'a>> {
        let mut events = Vec::with_capacity(self.estimate_events());
        while !self.raw.is_empty() {
            match TrackEvent::read(&mut self.raw, &mut self.running_status) {
                Ok(ev) => events.push(ev),
                Err(err) => {
                    self.raw = &mut [];
                    if cfg!(feature = "strict") {
                        Err(err).context(err_malformed!("malformed event"))?;
                    } else {
                        //Stop reading track silently on failure
                        break;
                    }
                }
            }
        }
        Ok(Track::new(events))
    }
}
impl<'a> Iterator for EventIter<'a> {
    type Item = Result<TrackEvent<'a>>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if !self.raw.is_empty() {
            match TrackEvent::read(&mut self.raw, &mut self.running_status) {
                Ok(ev) => Some(Ok(ev)),
                Err(err) => {
                    self.raw = &mut [];
                    if cfg!(feature = "strict") {
                        Some(Err(err).context(err_malformed!("malformed event")))
                    } else {
                        None
                    }
                }
            }
        } else {
            None
        }
    }
}

/// The order in which tracks should be laid out when playing back this SMF file.
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
#[repr(u8)]
pub enum Format {
    /// This file should have a single track only.
    ///
    /// If the `strict` feature is enabled, an error is raised if the format is
    /// `Format::SingleTrack` and there is not exactly one track.
    SingleTrack,
    /// This file has several tracks that should be played simultaneously.
    ///
    /// Usually the first track controls tempo and other song metadata.
    Parallel,
    /// This file has several tracks, each one a separate song.
    ///
    /// The tracks should be played sequentially, as completely separate MIDI tracks packaged
    /// within a single SMF file.
    Sequential,
}
impl Format {
    pub fn from_bits(bits: u16) -> Result<Format> {
        Ok(match bits {
            0 => Format::SingleTrack,
            1 => Format::Parallel,
            2 => Format::Sequential,
            _ => bail!(err_invalid!("invalid smf format")),
        })
    }

    pub fn as_bits(&self) -> u16 {
        *self as u8 as u16
    }
}

/// The timing for an SMF file.
/// This can be in ticks/beat or ticks/second.
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Timing {
    /// Specifies ticks/beat as a 15-bit integer.
    ///
    /// The length of a beat is not standard, so in order to fully describe the length of a MIDI
    /// tick the [`MetaMessage::Tempo`](enum.MetaMessage.html#Tempo.v) event should be present.
    Metrical(u16),
    /// Specifies ticks/second by dividing a second into frames and then into subframes.
    /// Therefore the length of of a tick is `1/fps/subframe`.
    Timecode(Fps, u8),
}
impl Timing {
    pub fn read(raw: &mut &mut [u8]) -> Result<Timing> {
        let raw = read_u16(raw).context(err_invalid!("expected midi timing"))?;
        if raw & 0x8000 != 0 {
            //Timecode
            let fps = -((raw >> 8) as i8);
            let subframe = (raw & 0xFF) as u8;
            Ok(Timing::Timecode(Fps::from_bits(fps as u8), subframe))
        } else {
            //Metrical
            Ok(Timing::Metrical(raw))
        }
    }

    pub fn encode(&self) -> [u8; 2] {
        match *self {
            Timing::Metrical(ticksperbeat) => ticksperbeat.to_be_bytes(),
            Timing::Timecode(framespersec, ticksperframe) => {
                [(-(framespersec as i8)) as u8, ticksperframe]
            }
        }
    }
}
