//! Specific to the SMF packaging of MIDI streams.

use crate::{
    event::Event,
    prelude::*,
    primitive::{Format, Timing},
    riff,
};

/// How many bytes per event to estimate when allocating memory for events.
///
/// A value that is too large (ie. too few bytes/event), will underallocate, while a value that is
/// too small (ie. too many bytes/event) will overallocate.
///
/// Usually, since memory is cheap it's better to overallocate, since reallocating the buffer may
/// result in costly memory moves.
///
/// Real-world tests show that without running status, the average is a little above 4 bytes/event,
/// and with running status enabled it's a little above 3 bytes/event.
/// This makes sense, since it's DeltaTime [+ Status] + Key + Velocity for NoteOn and NoteOff
/// events, which should make up the bulk of most MIDI files.
const EVENTS_PER_BYTE: f32 = 1.0 / 3.0;

/// How many events per byte to estimate when allocating memory for event data.
///
/// A value that is too large will overallocate space for bytes, while a value that's too small
/// will underallocate bytes.
///
/// Since the writer uses running status by default, a value a bit over `3` will allocate enough for
/// almost all cases (Except for eg. info tracks, which usually have a high byte/event count
/// because they contain text. However these tracks are small enough that reallocating doesn't
/// matter too much).
#[cfg(feature = "alloc")]
const BYTES_PER_EVENT: f32 = 3.4;

/// How many bytes must a MIDI body have in order to enable multithreading.
///
/// When writing, the MIDI body size is estimated from the event count using `BYTES_PER_EVENT`.
#[cfg(feature = "parallel")]
const PARALLEL_ENABLE_THRESHOLD: usize = 3 * 1024;

#[cfg(feature = "alloc")]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Smf<'a> {
    pub header: Header,
    pub tracks: Vec<Vec<Event<'a>>>,
}
#[cfg(feature = "alloc")]
impl Smf<'_> {
    pub fn new(header: Header, tracks: Vec<Vec<Event>>) -> Smf {
        Smf { header, tracks }
    }

    pub fn parse(raw: &[u8]) -> Result<Smf> {
        let (header, tracks) = parse(raw)?;
        let track_count_hint = tracks.track_count_hint;
        let tracks = tracks.collect_events()?;
        validate_smf(&header, track_count_hint, tracks.len())?;
        Ok(Smf { header, tracks })
    }

    pub fn write<W: Write>(&self, out: &mut W) -> IoResult<W> {
        write(&self.header, &self.tracks, out)
    }

    #[cfg(feature = "std")]
    pub fn save<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        fn save_impl(smf: &Smf, path: &Path) -> io::Result<()> {
            smf.write(&mut crate::io::SeekWrap(File::create(path)?))
        }
        save_impl(self, path.as_ref())
    }
}

#[cfg(feature = "alloc")]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SmfBytemap<'a> {
    pub header: Header,
    pub tracks: Vec<Vec<(&'a [u8], Event<'a>)>>,
}
#[cfg(feature = "alloc")]
impl<'a> SmfBytemap<'a> {
    pub fn new(header: Header, tracks: Vec<Vec<(&'a [u8], Event<'a>)>>) -> SmfBytemap<'a> {
        SmfBytemap { header, tracks }
    }

    pub fn parse(raw: &[u8]) -> Result<SmfBytemap> {
        let (header, tracks) = parse(raw)?;
        let track_count_hint = tracks.track_count_hint;
        let tracks = tracks.collect_bytemapped()?;
        validate_smf(&header, track_count_hint, tracks.len())?;
        Ok(SmfBytemap { header, tracks })
    }

    pub fn write<W: Write>(&self, out: &mut W) -> IoResult<W> {
        write(
            &self.header,
            self.tracks
                .iter()
                .map(|bytemapped| bytemapped.iter().map(|(_b, ev)| ev)),
            out,
        )
    }

    #[cfg(feature = "std")]
    pub fn save<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        fn save_impl(smf: &SmfBytemap, path: &Path) -> io::Result<()> {
            smf.write(&mut crate::io::SeekWrap(File::create(path)?))
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

/// Encode and write the MIDI file into the given generic writer.
///
/// This function will bubble up errors from the underlying writer and produce `InvalidInput`
/// errors if the MIDI file is extremely large (like for example if there are more than 65535
/// tracks or chunk sizes are over 4GB).
///
/// This function will make use of multiple threads if the `std` feature is enabled.
pub fn write<'a, T, E, W>(header: &Header, tracks: T, out: &mut W) -> IoResult<W>
where
    T: IntoIterator<Item = E>,
    T::IntoIter: ExactSizeIterator + Clone + Send,
    E: IntoIterator<Item = &'a Event<'a>>,
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
        if (event_count as f32 * BYTES_PER_EVENT) > PARALLEL_ENABLE_THRESHOLD as f32 {
            use rayon::prelude::*;

            //Write out the tracks in parallel into several different buffers
            let mut track_chunks = Vec::new();
            tracks
                .collect::<Vec<_>>()
                .into_par_iter()
                .map(|track| {
                    let mut track_chunk = Vec::new();
                    Chunk::write_to_vec(track, &mut track_chunk)
                        .map_err(|msg| W::invalid_input(msg))?;
                    Ok(track_chunk)
                })
                .collect_into_vec(&mut track_chunks);

            //Write down the tracks sequentially and in order
            for result in track_chunks {
                let track_chunk = result?;
                out.write_all(&track_chunk)?;
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
            out.write_all(&buf)?;
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
        } else {
            //Last resort: do probe-writing.
            //Two passes are done: one to find out the size of the chunk and another to actually
            //write the chunk.
            for track in tracks {
                Chunk::write_probe(track, out)?;
            }
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Debug)]
struct ChunkIter<'a> {
    /// Starts at the current index, ends at EOF.
    raw: &'a [u8],
}
impl<'a> ChunkIter<'a> {
    fn new(raw: &'a [u8]) -> ChunkIter {
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
            if raw.len() == 0 {
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
    fn write_header<W: Write>(header: &Header, track_count: usize, out: &mut W) -> IoResult<W> {
        let mut header_chunk = [0; 4 + 4 + 6];
        let track_count = u16::try_from(track_count)
            .map_err(|_| W::invalid_input("track count exceeds 16 bit range"))?;
        let header = header.encode(track_count);
        header_chunk[0..4].copy_from_slice(&b"MThd"[..]);
        header_chunk[4..8].copy_from_slice(&(header.len() as u32).to_be_bytes()[..]);
        header_chunk[8..].copy_from_slice(&header[..]);
        out.write_all(&header_chunk[..])?;
        Ok(())
    }

    /// Write a single track chunk using the probe method.
    ///
    /// When probing, the chunk is written twice: one to find out the length of the chunk and again
    /// to actually write the chunk contents.
    fn write_probe<W: Write>(
        track: impl Iterator<Item = &'a Event<'a>> + Clone,
        out: &mut W,
    ) -> IoResult<W> {
        let mut counter = WriteCounter(0);
        Self::write_raw(track.clone(), &mut counter).map_err(W::invalid_input)?;
        let len = Self::check_len::<W, _>(counter.0)?;
        let mut head = [b'M', b'T', b'r', b'k', 0, 0, 0, 0];
        head[4..8].copy_from_slice(&len);
        out.write_all(&head)?;
        Self::write_raw(track, out)?;
        Ok(())
    }

    /// Write a single chunk using the seek method.
    ///
    /// The chunk is written once, then the writer is seeked back and the chunk length is written
    /// last.
    fn write_seek<W: Write + Seek>(
        track: impl Iterator<Item = &'a Event<'a>>,
        out: &mut W,
    ) -> IoResult<W> {
        out.write_all(b"MTrk\0\0\0\0")?;
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
        track: impl Iterator<Item = &'a Event<'a>>,
        out: &mut Vec<u8>,
    ) -> IoResult<Vec<u8>> {
        let cap = (track.size_hint().0 as f32 * BYTES_PER_EVENT) as usize;
        out.clear();
        out.reserve(8 + cap);
        out.extend_from_slice(b"MTrk\0\0\0\0");
        Self::write_raw(track, out)?;
        let len = Self::check_len::<Vec<u8>, _>(out.len() - 8)?;
        out[4..8].copy_from_slice(&len);
        Ok(())
    }

    /// Utility method. Iterate over the events of a track and write them out.
    fn write_raw<W: Write>(track: impl Iterator<Item = &'a Event<'a>>, out: &mut W) -> IoResult<W> {
        let mut running_status = None;
        for ev in track {
            ev.write(&mut running_status, out)?;
        }
        Ok(())
    }

    /// Utility method. Given an arbitrary-width length, fit it into a 32-bit big-endian integer,
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

/// A MIDI file header.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Header {
    pub format: Format,
    pub timing: Timing,
}
impl Header {
    pub fn new(format: Format, timing: Timing) -> Header {
        Header { format, timing }
    }

    /// Read both the header and the track count.
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

/// An iterator over the tracks in a Standard Midi File.
pub struct TrackIter<'a> {
    chunks: ChunkIter<'a>,
    track_count_hint: u16,
}
impl<'a> TrackIter<'a> {
    pub fn unread(&self) -> &'a [u8] {
        self.chunks.raw
    }

    #[cfg(feature = "alloc")]
    pub fn collect_events(self) -> Result<Vec<Vec<Event<'a>>>> {
        self.generic_collect(EventIter::to_vec)
    }

    #[cfg(feature = "alloc")]
    pub fn collect_bytemapped(self) -> Result<Vec<Vec<(&'a [u8], Event<'a>)>>> {
        self.generic_collect(|events| events.bytemapped().to_vec())
    }

    #[cfg(feature = "alloc")]
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

    fn size_hint(&self) -> (usize, Option<usize>) {
        (
            self.track_count_hint as usize,
            Some(self.track_count_hint as usize),
        )
    }

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
                            break Some(
                                Err(err)
                                    .context(err_malformed!("invalid chunk"))
                                    .map_err(|err| err.into()),
                            );
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
    fn new(raw: &[u8]) -> EventIterGeneric<T> {
        EventIterGeneric {
            raw,
            running_status: None,
            _kind: PhantomData,
        }
    }

    /// Get the remaining unread bytes.
    fn unread(&self) -> &'a [u8] {
        self.raw
    }

    /// Get the current running status of the track.
    fn running_status(&self) -> Option<u8> {
        self.running_status
    }

    /// Modify the current running status of the track.
    fn running_status_mut(&mut self) -> &mut Option<u8> {
        &mut self.running_status
    }

    fn estimate_bytes(&self) -> usize {
        (self.raw.len() as f32 * EVENTS_PER_BYTE) as usize
    }

    #[cfg(feature = "alloc")]
    fn to_vec(mut self) -> Result<Vec<T::Event>> {
        let mut events = Vec::with_capacity(self.estimate_bytes());
        while self.raw.len() > 0 {
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
    fn next(&mut self) -> Option<Self::Item> {
        if self.raw.len() > 0 {
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

/// An iterator of events over a single track.
/// Allows deferring the parsing of tracks for later, on an on-demand basis.
///
/// This is the best option when traversing a track once or avoiding memory allocations.
/// This `struct` is very light, so it can be cloned freely.
#[derive(Clone, Debug)]
pub struct EventIter<'a> {
    inner: EventIterGeneric<'a, Self>,
}
impl<'a> EventKind<'a> for EventIter<'a> {
    type Event = Event<'a>;
    fn read_ev(raw: &mut &'a [u8], rs: &mut Option<u8>) -> Result<Event<'a>> {
        Event::read(raw, rs)
    }
}
impl<'a> EventIter<'a> {
    pub fn new(raw: &[u8]) -> EventIter {
        EventIter {
            inner: EventIterGeneric::new(raw),
        }
    }

    /// Get the remaining unread bytes.
    pub fn unread(&self) -> &'a [u8] {
        self.inner.unread()
    }

    /// Get the current running status of the track.
    pub fn running_status(&self) -> Option<u8> {
        self.inner.running_status()
    }

    /// Modify the current running status of the track.
    pub fn running_status_mut(&mut self) -> &mut Option<u8> {
        self.inner.running_status_mut()
    }

    pub fn bytemapped(self) -> EventBytemapIter<'a> {
        EventBytemapIter {
            inner: EventIterGeneric {
                raw: self.inner.raw,
                running_status: self.inner.running_status,
                _kind: PhantomData,
            },
        }
    }

    #[cfg(feature = "alloc")]
    pub fn to_vec(self) -> Result<Vec<Event<'a>>> {
        self.inner.to_vec()
    }
}
impl<'a> Iterator for EventIter<'a> {
    type Item = Result<Event<'a>>;
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

pub struct EventBytemapIter<'a> {
    inner: EventIterGeneric<'a, Self>,
}
impl<'a> EventKind<'a> for EventBytemapIter<'a> {
    type Event = (&'a [u8], Event<'a>);
    fn read_ev(raw: &mut &'a [u8], rs: &mut Option<u8>) -> Result<Self::Event> {
        Event::read_bytemap(raw, rs)
    }
}
impl<'a> EventBytemapIter<'a> {
    pub fn new(raw: &[u8]) -> EventBytemapIter {
        EventBytemapIter {
            inner: EventIterGeneric::new(raw),
        }
    }

    /// Get the remaining unread bytes.
    pub fn unread(&self) -> &'a [u8] {
        self.inner.unread()
    }

    /// Get the current running status of the track.
    pub fn running_status(&self) -> Option<u8> {
        self.inner.running_status()
    }

    /// Modify the current running status of the track.
    pub fn running_status_mut(&mut self) -> &mut Option<u8> {
        self.inner.running_status_mut()
    }

    #[cfg(feature = "alloc")]
    pub fn to_vec(self) -> Result<Vec<(&'a [u8], Event<'a>)>> {
        self.inner.to_vec()
    }
}
impl<'a> Iterator for EventBytemapIter<'a> {
    type Item = Result<(&'a [u8], Event<'a>)>;
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}
