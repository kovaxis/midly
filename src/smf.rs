//! Specific to the SMF packaging of MIDI streams.

use crate::{
    event::Event,
    prelude::*,
    primitive::{Format, Timing},
    riff,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Smf<'a> {
    pub header: Header,
    pub tracks: Vec<Vec<Event<'a>>>,
}
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

    pub fn write<W: Write>(&self, out: &mut W) -> IoResult<()> {
        write(&self.header, self.tracks.iter(), out)
    }

    pub fn save<P: AsRef<Path>>(&self, path: P) -> IoResult<()> {
        fn save_impl(smf: &Smf, path: &Path) -> IoResult<()> {
            smf.write(&mut File::create(path)?)
        }
        save_impl(self, path.as_ref())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SmfBytemap<'a> {
    pub header: Header,
    pub tracks: Vec<Vec<(&'a [u8], Event<'a>)>>,
}
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

    pub fn write<W: Write>(&self, out: &mut W) -> IoResult<()> {
        write(
            &self.header,
            self.tracks
                .iter()
                .map(|track| track.iter().map(|(_b, ev)| ev)),
            out,
        )
    }

    pub fn save<P: AsRef<Path>>(&self, path: P) -> IoResult<()> {
        fn save_impl(smf: &SmfBytemap, path: &Path) -> IoResult<()> {
            smf.write(&mut File::create(path)?)
        }
        save_impl(self, path.as_ref())
    }
}

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
    let raw = riff::unwrap(raw).unwrap_or(raw);
    let mut chunks = ChunkIter::read(raw);
    let (header, track_count) = match chunks.next() {
        Some(maybe_chunk) => match maybe_chunk.context(err_invalid!("invalid midi header"))? {
            Chunk::Header(header, track_count) => Ok((header, track_count)),
            Chunk::Track(_) => Err(err_invalid!("expected header, found track")),
        },
        None => Err(err_invalid!("no header chunk")),
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
#[cfg(feature = "std")]
pub fn write<'a, W: Write>(
    header: &Header,
    tracks: impl Iterator<Item = impl IntoIterator<Item = &'a Event<'a>>> + ExactSizeIterator,
    out: &mut W,
) -> IoResult<()> {
    //Write the header first
    Chunk::write_header(header, tracks.len(), out)?;

    //Try to write the file in parallel
    /*
    #[cfg(feature = "std")]
    {
        if T::USE_MULTITHREADING {
            use rayon::prelude::*;

            //Write out the tracks in parallel into several different buffers
            let track_chunks = self
                .tracks
                .par_iter()
                .map(|track| {
                    let mut track_chunk = Vec::with_capacity(8 * 1024);
                    Chunk::write_track(track, &mut track_chunk)?;
                    Ok(track_chunk)
                })
                .collect::<IoResult<Vec<_>>>()?;

            //Write down the tracks sequentially and in order
            for track_chunk in track_chunks {
                out.write_all(&track_chunk)?;
            }
            return Ok(());
        }
    }
    */

    //Fall back to writing the file serially
    //Write tracks into a reusable buffer before writing them out
    let mut track_chunk = Vec::with_capacity(8 * 1024);
    for track in tracks {
        //Write tracks into a buffer first so that chunk lengths can be written
        Chunk::write_track(track, &mut track_chunk)?;
        out.write_all(&track_chunk[..])?;
        track_chunk.clear();
    }
    Ok(())
}

#[derive(Copy, Clone, Debug)]
struct ChunkIter<'a> {
    /// Starts at the current index, ends at EOF.
    raw: &'a [u8],
}
impl<'a> ChunkIter<'a> {
    fn read(raw: &'a [u8]) -> ChunkIter {
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
    #[cfg(feature = "std")]
    fn write_header<W: Write>(header: &Header, track_count: usize, out: &mut W) -> IoResult<()> {
        let mut header_chunk = [0; 4 + 4 + 6];
        let track_count = u16::try_from(track_count).map_err(|_| {
            IoError::new(
                io::ErrorKind::InvalidInput,
                "track count exceeds 16 bit range",
            )
        })?;
        let header = header.encode(track_count);
        header_chunk[0..4].copy_from_slice(&b"MThd"[..]);
        header_chunk[4..8].copy_from_slice(&(header.len() as u32).to_be_bytes()[..]);
        header_chunk[8..].copy_from_slice(&header[..]);
        out.write_all(&header_chunk[..])?;
        Ok(())
    }

    /// Write a track chunk into a `Vec`.
    ///
    /// The `Vec` should be empty.
    #[cfg(feature = "std")]
    fn write_track(
        track: impl IntoIterator<Item = &'a Event<'a>>,
        out: &mut Vec<u8>,
    ) -> IoResult<()> {
        out.extend_from_slice(b"MTrk\0\0\0\0");
        let mut running_status = None;
        let events = track.into_iter();
        out.reserve(events.size_hint().0);
        for ev in events {
            ev.write(&mut running_status, out)?;
        }
        let len = u32::try_from(out.len() - 8).map_err(|_| {
            IoError::new(
                io::ErrorKind::InvalidInput,
                "midi chunk size exceeds 32 bit range",
            )
        })?;
        out[4..8].copy_from_slice(&len.to_be_bytes());
        Ok(())
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
    #[cfg(feature = "std")]
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

    pub fn collect_events(self) -> Result<Vec<Vec<Event<'a>>>> {
        //Attempt to use multiple threads if possible and enabled
        /*#[cfg(feature = "std")]
        {
            if T::USE_MULTITHREADING {
                use rayon::prelude::*;

                let mut chunk_vec = Vec::with_capacity(self.count_hint as usize);
                chunk_vec.extend(self);
                return chunk_vec
                    .into_par_iter()
                    .filter_map(Chunk::parse_into_track)
                    .collect::<Result<Vec<T>>>();
            }
        }*/
        //Fall back to single-threaded
        let mut tracks = Vec::with_capacity(self.track_count_hint as usize);
        for chunk_result in self {
            tracks.push(chunk_result?.collect_events()?);
        }
        Ok(tracks)
    }

    pub fn collect_bytemapped(self) -> Result<Vec<Vec<(&'a [u8], Event<'a>)>>> {
        //Attempt to use multiple threads if possible and enabled
        /*#[cfg(feature = "std")]
        {
            if T::USE_MULTITHREADING {
                use rayon::prelude::*;

                let mut chunk_vec = Vec::with_capacity(self.count_hint as usize);
                chunk_vec.extend(self);
                return chunk_vec
                    .into_par_iter()
                    .filter_map(Chunk::parse_into_track)
                    .collect::<Result<Vec<T>>>();
            }
        }*/
        //Fall back to single-threaded
        let mut tracks = Vec::with_capacity(self.track_count_hint as usize);
        for chunk_result in self {
            tracks.push(chunk_result?.collect_bytemapped()?);
        }
        Ok(tracks)
    }
}
impl<'a> Iterator for TrackIter<'a> {
    type Item = Result<EventIter<'a>>;

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.track_count_hint as usize, None)
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

/// An iterator of events over a single track.
/// Allows deferring the parsing of tracks for later, on an on-demand basis.
///
/// This is the best option when traversing a track once or avoiding memory allocations.
/// This `struct` is very light, so it can be cloned freely.
#[derive(Clone, Debug)]
pub struct EventIter<'a> {
    raw: &'a [u8],
    running_status: Option<u8>,
}
impl<'a> EventIter<'a> {
    pub fn new(raw: &[u8]) -> EventIter {
        EventIter {
            raw,
            running_status: None,
        }
    }

    /// Get the remaining unread bytes.
    pub fn unread(&self) -> &'a [u8] {
        self.raw
    }

    /// Get the current running status of the track.
    pub fn running_status(&self) -> Option<u8> {
        self.running_status
    }

    /// Modify the current running status of the track.
    pub fn running_status_mut(&mut self) -> &mut Option<u8> {
        &mut self.running_status
    }

    pub fn collect_events(self) -> Result<Vec<Event<'a>>> {
        let mut events = Vec::with_capacity(self.raw.len() / 2);
        let mut track_iter = EventIter::new(self.raw);
        while let Some(ev) = track_iter.next() {
            events.push(ev?);
        }
        Ok(events)
    }

    pub fn collect_bytemapped(self) -> Result<Vec<(&'a [u8], Event<'a>)>> {
        let mut events = Vec::with_capacity(self.raw.len() / 2);
        let mut track_iter = EventIter::new(self.raw);
        let mut last_raw = track_iter.unread();
        while let Some(ev) = track_iter.next() {
            let raw_ev = &last_raw[..last_raw.len() - track_iter.unread().len()];
            last_raw = track_iter.unread();
            events.push((raw_ev, ev?));
        }
        Ok(events)
    }
}
impl<'a> Iterator for EventIter<'a> {
    type Item = Result<Event<'a>>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.raw.len() > 0 {
            let read_result = Event::read(&mut self.raw, &mut self.running_status);
            if cfg!(feature = "strict") {
                Some(read_result.context(err_malformed!("malformed event")))
            } else {
                match read_result {
                    Ok(ev) => Some(Ok(ev)),
                    //Ignore errors
                    Err(_err) => None,
                }
            }
        } else {
            None
        }
    }
}
