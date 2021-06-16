//! Specific to the SMF packaging of MIDI streams.

use crate::{
    event::TrackEvent,
    prelude::*,
    primitive::{Format, Timing},
    riff,
};

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
/// because they contain text. However these tracks are small enough that reallocating doesn't
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
pub type Track<'a> = Vec<TrackEvent<'a>>;

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
    ///
    /// Each track consists simply of a list of events (ie. there is no track metadata).
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
    pub fn parse(raw: &[u8]) -> Result<Smf> {
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
            smf.write(&mut IoWrap(File::create(path)?))
        }
        save_impl(self, path.as_ref())
    }

    /// Remove any lifetimed data from this event to create an `Smf` with `'static`
    /// lifetime that can be stored and moved everywhere, solving borrow checker issues.
    ///
    /// This method creates a copy of the `Smf` structure. See the `make_static` method for an
    /// in-place solution.
    ///
    /// WARNING: Any bytestrings, including meta messages, SysEx dumps and escape sequences will be
    /// replaced by empty bytestrings.
    pub fn to_static(&self) -> Smf<'static> {
        self.clone().make_static()
    }

    /// Remove any lifetimed data from this event to create an `Smf` with `'static`
    /// lifetime that can be stored and moved everywhere, solving borrow checker issues.
    ///
    /// This method consumes the `Smf` structure, reusing the backing memory.
    ///
    /// WARNING: Any bytestrings, including meta messages, SysEx dumps and escape sequences will be
    /// replaced by empty bytestrings.
    pub fn make_static(mut self) -> Smf<'static> {
        for track in self.tracks.iter_mut() {
            for ev in track.iter_mut() {
                *ev = ev.to_static();
            }
        }
        unsafe { mem::transmute::<Smf<'a>, Smf<'static>>(self) }
    }
}

/// A track, represented as a `Vec` of events along with their originating bytes.
///
/// This type alias is only available with the `alloc` feature enabled.
#[cfg(feature = "alloc")]
pub type BytemappedTrack<'a> = Vec<(&'a [u8], TrackEvent<'a>)>;

/// A `.mid` Standard Midi File, but keeps a mapping to the raw bytes that make up each event.
///
/// This type is only available with the `alloc` feature enabled.
#[cfg(feature = "alloc")]
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct SmfBytemap<'a> {
    /// The header of this file.
    pub header: Header,
    /// A list of tracks, along with the bytemap of their events.
    pub tracks: Vec<BytemappedTrack<'a>>,
}
#[cfg(feature = "alloc")]
impl<'a> SmfBytemap<'a> {
    /// Create a new empty `SmfBytemap` with zero tracks, using the given header.
    #[inline]
    pub fn new(header: Header) -> SmfBytemap<'a> {
        SmfBytemap {
            header,
            tracks: vec![],
        }
    }

    /// Parse a Standard Midi File from its raw bytes, keeping a map to the original bytes that
    /// make up each event.
    pub fn parse(raw: &[u8]) -> Result<SmfBytemap> {
        let (header, tracks) = parse(raw)?;
        let track_count_hint = tracks.track_count_hint;
        let tracks = tracks.collect_bytemapped()?;
        validate_smf(&header, track_count_hint, tracks.len())?;
        Ok(SmfBytemap { header, tracks })
    }

    /// Encodes and writes the *events* (not the bytemap) to the given generic writer.
    #[inline]
    pub fn write<W: Write>(&self, out: &mut W) -> WriteResult<W> {
        write(
            &self.header,
            self.tracks
                .iter()
                .map(|bytemapped| bytemapped.iter().map(|(_b, ev)| ev)),
            out,
        )
    }

    /// Encodes and writes the *events* (not the bytemap) to the given `std::io::Write` writer.
    ///
    /// This function is only available with the `std` feature enabled.
    #[cfg(feature = "std")]
    #[inline]
    pub fn write_std<W: io::Write>(&self, out: W) -> io::Result<()> {
        write_std(
            &self.header,
            self.tracks
                .iter()
                .map(|bytemapped| bytemapped.iter().map(|(_b, ev)| ev)),
            out,
        )
    }

    /// Creates/overwrites the file at the given path and writes the *events* (not the bytemap) to
    /// it.
    ///
    /// This function is only available with the `std` feature enabled.
    #[cfg(feature = "std")]
    #[inline]
    pub fn save<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        /// A non-generic, non-inline function.
        /// This means that this function will be compiled and monomorphized once, and reused for
        /// every call to `save`.
        fn save_impl(smf: &SmfBytemap, path: &Path) -> io::Result<()> {
            smf.write(&mut IoWrap(File::create(path)?))
        }
        save_impl(self, path.as_ref())
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
pub fn parse(raw: &[u8]) -> Result<(Header, TrackIter)> {
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
    write(header, tracks, &mut IoWrap(out))
}

#[derive(Clone, Debug)]
struct ChunkIter<'a> {
    /// Starts at the current index, ends at EOF.
    raw: &'a [u8],
}
impl<'a> ChunkIter<'a> {
    #[inline]
    fn new(raw: &'a [u8]) -> ChunkIter {
        ChunkIter { raw }
    }

    #[inline]
    fn as_tracks(self, track_count_hint: u16) -> TrackIter<'a> {
        TrackIter {
            chunks: self,
            track_count_hint,
        }
    }
}
impl<'a> Iterator for ChunkIter<'a> {
    type Item = Result<Chunk<'a>>;
    #[inline]
    fn next(&mut self) -> Option<Result<Chunk<'a>>> {
        //Flip around option and result
        match Chunk::read(&mut self.raw) {
            Ok(Some(chunk)) => Some(Ok(chunk)),
            Ok(None) => None,
            Err(err) => {
                //Ensure `Chunk::read` isn't called again, by setting read pointer to EOF (len 0)
                //This is to prevent use of corrupted state (such as reading a new Chunk from the
                //middle of a malformed message)
                self.raw = &[];
                Some(Err(err))
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum Chunk<'a> {
    Header(Header, u16),
    Track(&'a [u8]),
}
impl<'a> Chunk<'a> {
    /// Should be called with a byte slice at least as large as the chunk (ideally until EOF).
    /// The slice will be modified to point to the next chunk.
    /// If we're *exactly* at EOF (slice length 0), returns a None signalling no more chunks.
    fn read(raw: &mut &'a [u8]) -> Result<Option<Chunk<'a>>> {
        Ok(loop {
            if raw.is_empty() {
                break None;
            }
            let id = raw
                .split_checked(4)
                .ok_or(err_invalid!("failed to read chunkid"))?;
            let len = u32::read(raw).context(err_invalid!("failed to read chunklen"))?;
            let chunkdata = match raw.split_checked(len as usize) {
                Some(chunkdata) => chunkdata,
                None => {
                    if cfg!(feature = "strict") {
                        bail!(err_malformed!("reached eof before chunk ended"));
                    } else {
                        //Just use the remainder of the file
                        mem::replace(raw, &[])
                    }
                }
            };
            match id {
                b"MThd" => {
                    let (header, track_count) = Header::read(chunkdata)?;
                    break Some(Chunk::Header(header, track_count));
                }
                b"MTrk" => {
                    break Some(Chunk::Track(chunkdata));
                }
                //Unknown chunk, just ignore and read the next one
                _ => (),
            }
        })
    }

    /// Write a header chunk into a writer.
    fn write_header<W: Write>(header: &Header, track_count: usize, out: &mut W) -> WriteResult<W> {
        let mut header_chunk = [0; 4 + 4 + 6];
        let track_count = u16::try_from(track_count)
            .map_err(|_| W::invalid_input("track count exceeds 16 bit range"))?;
        let header = header.encode(track_count);
        header_chunk[0..4].copy_from_slice(&b"MThd"[..]);
        header_chunk[4..8].copy_from_slice(&(header.len() as u32).to_be_bytes()[..]);
        header_chunk[8..].copy_from_slice(&header[..]);
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
    fn read(mut raw: &[u8]) -> Result<(Header, u16)> {
        let format = Format::read(&mut raw)?;
        let track_count = u16::read(&mut raw)?;
        let timing = Timing::read(&mut raw)?;
        Ok((Header::new(format, timing), track_count))
    }
    fn encode(&self, track_count: u16) -> [u8; 6] {
        let mut bytes = [0; 6];
        bytes[0..2].copy_from_slice(&self.format.encode()[..]);
        bytes[2..4].copy_from_slice(&track_count.to_be_bytes()[..]);
        bytes[4..6].copy_from_slice(&self.timing.encode()[..]);
        bytes
    }
}

/// An iterator over all *tracks* in a Standard Midi File.
/// Created by the [`parse`](fn.parse.html) function.
///
/// This type is always available, even in `no_std` environments.
#[derive(Clone, Debug)]
pub struct TrackIter<'a> {
    chunks: ChunkIter<'a>,
    track_count_hint: u16,
}
impl<'a> TrackIter<'a> {
    /// Create an event iterator from raw SMF bytes, excluding the header.
    ///
    /// The main way to obtain raw SMF without a header is the [`unread`](#method.unread) method.
    #[inline]
    pub fn new(raw: &[u8]) -> TrackIter {
        TrackIter {
            chunks: ChunkIter::new(raw),
            track_count_hint: 0,
        }
    }

    /// Peek at the remaining unparsed bytes in the file.
    #[inline]
    pub fn unread(&self) -> &'a [u8] {
        self.chunks.raw
    }

    /// Parse and collect the remaining unparsed tracks into a `Vec` of tracks.
    ///
    /// This function is only available with the `alloc` feature enabled.
    #[cfg(feature = "alloc")]
    pub fn collect_tracks(self) -> Result<Vec<Track<'a>>> {
        self.generic_collect(EventIter::into_vec)
    }

    /// Parse and collect the remaining unparsed tracks into a `Vec` of tracks, keeping a mapping
    /// to the original bytes that make up each event.
    ///
    /// This function is only available with the `alloc` feature enabled.
    #[cfg(feature = "alloc")]
    pub fn collect_bytemapped(self) -> Result<Vec<BytemappedTrack<'a>>> {
        self.generic_collect(|events| events.bytemapped().into_vec())
    }

    #[cfg(feature = "alloc")]
    #[inline]
    fn generic_collect<T: Send + 'a>(
        self,
        collect: impl Fn(EventIter<'a>) -> Result<Vec<T>> + Send + Sync,
    ) -> Result<Vec<Vec<T>>> {
        //Attempt to use multiple threads if possible and advantageous
        #[cfg(feature = "parallel")]
        {
            if self.unread().len() >= PARALLEL_ENABLE_THRESHOLD {
                use rayon::prelude::*;

                let chunk_vec = self.collect::<Result<Vec<_>>>()?;
                return chunk_vec
                    .into_par_iter()
                    .map(collect)
                    .collect::<Result<Vec<Vec<T>>>>();
            }
        }
        //Fall back to single-threaded
        self.map(|r| r.and_then(&collect))
            .collect::<Result<Vec<Vec<T>>>>()
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

trait EventKind<'a> {
    type Event: 'a;
    fn read_ev(raw: &mut &'a [u8], running_status: &mut Option<u8>) -> Result<Self::Event>;
}

#[derive(Clone, Debug)]
struct EventIterGeneric<'a, T> {
    raw: &'a [u8],
    running_status: Option<u8>,
    _kind: PhantomData<T>,
}
impl<'a, T: EventKind<'a>> EventIterGeneric<'a, T> {
    #[inline]
    fn new(raw: &[u8]) -> EventIterGeneric<T> {
        EventIterGeneric {
            raw,
            running_status: None,
            _kind: PhantomData,
        }
    }

    /// Get the remaining unread bytes.
    #[inline]
    fn unread(&self) -> &'a [u8] {
        self.raw
    }

    /// Get the current running status of the track.
    #[inline]
    fn running_status(&self) -> Option<u8> {
        self.running_status
    }

    /// Modify the current running status of the track.
    #[inline]
    fn running_status_mut(&mut self) -> &mut Option<u8> {
        &mut self.running_status
    }

    #[cfg(feature = "alloc")]
    #[inline]
    fn estimate_events(&self) -> usize {
        (self.raw.len() as f32 * BYTES_TO_EVENTS) as usize
    }

    #[cfg(feature = "alloc")]
    #[inline]
    fn into_vec(mut self) -> Result<Vec<T::Event>> {
        let mut events = Vec::with_capacity(self.estimate_events());
        while !self.raw.is_empty() {
            match T::read_ev(&mut self.raw, &mut self.running_status) {
                Ok(ev) => events.push(ev),
                Err(err) => {
                    self.raw = &[];
                    if cfg!(feature = "strict") {
                        Err(err).context(err_malformed!("malformed event"))?;
                    } else {
                        //Stop reading track silently on failure
                        break;
                    }
                }
            }
        }
        Ok(events)
    }
}
impl<'a, T: EventKind<'a>> Iterator for EventIterGeneric<'a, T> {
    type Item = Result<T::Event>;
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if !self.raw.is_empty() {
            match T::read_ev(&mut self.raw, &mut self.running_status) {
                Ok(ev) => Some(Ok(ev)),
                Err(err) => {
                    self.raw = &[];
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

/// An iterator over the events of a single track.
/// Yielded by the [`TrackIter`](struct.TrackIter.html) iterator.
///
/// This iterator is lazy, it parses events as it goes, and therefore produces `Result<TrackEvent>>`
/// rather than `TrackEvent`.
///
/// This type is always available, even in `no_std` environments.
#[derive(Clone, Debug)]
pub struct EventIter<'a> {
    inner: EventIterGeneric<'a, Self>,
}
impl<'a> EventKind<'a> for EventIter<'a> {
    type Event = TrackEvent<'a>;
    #[inline]
    fn read_ev(raw: &mut &'a [u8], rs: &mut Option<u8>) -> Result<TrackEvent<'a>> {
        TrackEvent::read(raw, rs)
    }
}
impl<'a> EventIter<'a> {
    /// Create an event iterator from raw track bytes.
    ///
    /// It can be hard to obtain raw track bytes.
    /// Usually these raw track bytes are obtained from the [`unread`](#method.unread) method on an
    /// event iterator.
    #[inline]
    pub fn new(raw: &[u8]) -> EventIter {
        EventIter {
            inner: EventIterGeneric::new(raw),
        }
    }

    /// Get the remaining unparsed event bytes.
    #[inline]
    pub fn unread(&self) -> &'a [u8] {
        self.inner.unread()
    }

    /// Get the current running status of the track.
    #[inline]
    pub fn running_status(&self) -> Option<u8> {
        self.inner.running_status()
    }

    /// Modify the current running status of the track.
    #[inline]
    pub fn running_status_mut(&mut self) -> &mut Option<u8> {
        self.inner.running_status_mut()
    }

    /// Make this event iterator keep track of the raw bytes that make up each event.
    #[inline]
    pub fn bytemapped(self) -> EventBytemapIter<'a> {
        EventBytemapIter {
            inner: EventIterGeneric {
                raw: self.inner.raw,
                running_status: self.inner.running_status,
                _kind: PhantomData,
            },
        }
    }

    /// Collects the remaining unparsed events into a `Track`.
    ///
    /// This function is a smarter version of `Iterator::collect`, as it guesses allocations and
    /// is usually optimized better than its naive counterpart.
    ///
    /// This function is only available with the `alloc` feature enabled.
    #[cfg(feature = "alloc")]
    #[inline]
    pub fn into_vec(self) -> Result<Track<'a>> {
        self.inner.into_vec()
    }
}
impl<'a> Iterator for EventIter<'a> {
    type Item = Result<TrackEvent<'a>>;
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

/// An iterator over the events of a single track that keeps track of the raw bytes that make up
/// each event.
/// Created by the [`EventIter::bytemapped`](struct.EventIter.html#method.bytemapped) method.
///
/// This iterator is lazy, it parses events as it goes, and therefore produces
/// `Result<(&[u8], TrackEvent)>>` rather than just `(&[u8], TrackEvent)`.
///
/// This type is always available, even in `no_std` environments.
#[derive(Clone, Debug)]
pub struct EventBytemapIter<'a> {
    inner: EventIterGeneric<'a, Self>,
}
impl<'a> EventKind<'a> for EventBytemapIter<'a> {
    type Event = (&'a [u8], TrackEvent<'a>);
    #[inline]
    fn read_ev(raw: &mut &'a [u8], rs: &mut Option<u8>) -> Result<Self::Event> {
        TrackEvent::read_bytemap(raw, rs)
    }
}
impl<'a> EventBytemapIter<'a> {
    /// Create an event iterator from raw track bytes.
    ///
    /// It can be hard to obtain raw track bytes.
    /// Usually these raw track bytes are obtained from the [`unread`](#method.unread) method on an
    /// event iterator.
    #[inline]
    pub fn new(raw: &[u8]) -> EventBytemapIter {
        EventBytemapIter {
            inner: EventIterGeneric::new(raw),
        }
    }

    /// Get the remaining unparsed event bytes.
    #[inline]
    pub fn unread(&self) -> &'a [u8] {
        self.inner.unread()
    }

    /// Get the current running status of the track.
    #[inline]
    pub fn running_status(&self) -> Option<u8> {
        self.inner.running_status()
    }

    /// Modify the current running status of the track.
    #[inline]
    pub fn running_status_mut(&mut self) -> &mut Option<u8> {
        self.inner.running_status_mut()
    }

    /// Stop collecting bytemap information for any remaining events.
    #[inline]
    pub fn not_bytemapped(self) -> EventIter<'a> {
        EventIter {
            inner: EventIterGeneric {
                raw: self.inner.raw,
                running_status: self.inner.running_status,
                _kind: PhantomData,
            },
        }
    }

    /// Collects the remaining unparsed events into a `Vec<(&[u8], TrackEvent)>`.
    ///
    /// This function is a smarter version of `Iterator::collect`, as it guesses allocations and
    /// is usually optimized better than its naive counterpart.
    ///
    /// This function is only available with the `alloc` feature enabled.
    #[cfg(feature = "alloc")]
    #[inline]
    pub fn into_vec(self) -> Result<Vec<(&'a [u8], TrackEvent<'a>)>> {
        self.inner.into_vec()
    }
}
impl<'a> Iterator for EventBytemapIter<'a> {
    type Item = Result<(&'a [u8], TrackEvent<'a>)>;
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}
