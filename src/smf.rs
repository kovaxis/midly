use crate::{
    event::Event,
    prelude::*,
    primitive::{Format, Timing},
};

/// Represents a Standard Midi File (.mid and .midi files).
/// Yields `TrackRepr` on a Vec, allowing for customization on what is stored in a track.
/// References an outside byte array.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Smf<'a, T: TrackRepr<'a> = Vec<Event<'a>>> {
    pub header: Header,
    pub tracks: Vec<T>,
    _lifetime: PhantomData<&'a ()>,
}
impl Smf<'_> {
    /// Preferred method to parse raw bytes into an `Smf` struct.
    ///
    /// This method parses the events of each track immediately into a `Vec<Event>`.
    ///
    /// If you wish to defer the parsing for later or want the raw source bytes for every event,
    /// check the other `parse_*` methods.
    pub fn parse(raw: &[u8]) -> Result<Smf> {
        Smf::read(raw)
    }

    /// Parses tracks into events and additionally provides the *source bytes* for each event.
    /// This can be used to forward the raw event bytes to a MIDI device/synthesizer.
    ///
    /// Same with the `parse` method, this method parses the events immediately into a
    /// `Vec<(&[u8], Event)>`.
    pub fn parse_with_bytemap(raw: &[u8]) -> Result<Smf<Vec<(&[u8], Event)>>> {
        Smf::read(raw)
    }

    /// Does *not* parse events, only recognizes the file and splits up the tracks, providing an
    /// iterator that lazily parses events.
    ///
    /// This method can be used to save memory or processing time, but it is usually not worth it
    /// except in very niche cases.
    /// Because the other `parse` methods use multiple threads to parse tracks, the use of this
    /// method is discouraged as it carries a performance penalty unless done correctly.
    pub fn parse_lazy(raw: &[u8]) -> Result<Smf<TrackIter>> {
        Smf::read(raw)
    }
}
impl<'a, T: TrackRepr<'a>> Smf<'a, T> {
    /// Create a new SMF from its raw parts.
    pub fn new(header: Header, tracks: Vec<T>) -> Result<Smf<'a, T>> {
        Ok(Smf {
            header,
            tracks,
            _lifetime: PhantomData,
        })
    }
    
    /// Generic `read` method.
    ///
    /// Prefer the `parse` methods instead, which handle generics for you.
    pub fn read(raw: &'a [u8]) -> Result<Smf<'a, T>> {
        let mut chunks = ChunkIter::read(raw);
        let (header, track_count) = match chunks.next() {
            Some(maybe_chunk) => match maybe_chunk.context(err_invalid("invalid midi header"))? {
                Chunk::Header(header, track_count) => Ok((header, track_count)),
                Chunk::Track(_) => Err(err_invalid("expected header, found track")),
            },
            None => Err(err_invalid("empty file")),
        }?;
        let tracks = chunks.parse_as_tracks(track_count)?;
        if !cfg!(feature = "lenient") {
            ensure!(
                track_count as usize == tracks.len(),
                err_malformed("file has a different amount of tracks than declared")
            );
            ensure!(
                header.format != Format::SingleTrack || track_count == 1,
                err_malformed("singletrack format file has multiple tracks")
            );
        }
        Ok(Smf::new(header, tracks)?)
    }
    
    /// Encode and write the MIDI file into the given generic writer.
    ///
    /// This function will bubble up errors from the underlying writer and produce `InvalidInput`
    /// errors if the MIDI file is extremely large (like for example if there are more than 65535
    /// tracks or chunk sizes are over 4GB).
    #[cfg(feature = "std")]
    pub fn write<W: Write>(&self, out: &mut W) -> IoResult<()> {
        //Write header
        let mut header_chunk = [0; 4 + 4 + 6];
        let track_count = u16::try_from(self.tracks.len()).map_err(|_| {
            IoError::new(
                io::ErrorKind::InvalidInput,
                "track count exceeds 16 bit range",
            )
        })?;
        let header = self.header.encode(track_count);
        header_chunk[0..4].copy_from_slice(&b"MThd"[..]);
        header_chunk[4..8].copy_from_slice(&(header.len() as u32).to_be_bytes()[..]);
        header_chunk[8..].copy_from_slice(&header[..]);
        out.write_all(&header_chunk[..])?;
        //Write tracks
        let mut track_data = Vec::with_capacity(16 * 1024);
        for track in self.tracks.iter() {
            //Write tracks into a buffer first so that chunk lengths can be written
            track_data.clear();
            track_data.extend_from_slice(b"MTrk\0\0\0\0");
            track.write(&mut track_data)?;
            let len = u32::try_from(track_data.len() - 8).map_err(|_| {
                IoError::new(
                    io::ErrorKind::InvalidInput,
                    "midi chunk size exceeds 32 bit range",
                )
            })?;
            track_data[4..8].copy_from_slice(&len.to_be_bytes());
            out.write_all(&track_data[..])?;
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
    fn read(raw: &'a [u8]) -> ChunkIter {
        ChunkIter { raw }
    }

    /// Interpret the remaining chunks as tracks.
    fn parse_as_tracks<T: TrackRepr<'a>>(self, track_count_hint: u16) -> Result<Vec<T>> {
        //Attempt to use multiple threads if possible and enabled
        #[cfg(feature = "std")]
        {
            if T::USE_MULTITHREADING {
                use rayon::prelude::*;

                let mut chunk_vec = Vec::with_capacity(track_count_hint as usize);
                chunk_vec.extend(self);
                return chunk_vec
                    .into_par_iter()
                    .filter_map(Chunk::parse_into_track)
                    .collect::<Result<Vec<T>>>();
            }
        }
        //Fall back to single-threaded
        let mut tracks = Vec::with_capacity(track_count_hint as usize);
        for chunk_result in self {
            if let Some(track) = Chunk::parse_into_track(chunk_result) {
                tracks.push(track?);
            }
        }
        Ok(tracks)
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
                .ok_or(err_invalid("failed to read chunkid"))?;
            let len = u32::read(raw).context(err_invalid("failed to read chunklen"))?;
            let chunkdata = match raw.split_checked(len as usize) {
                Some(chunkdata) => chunkdata,
                None => {
                    if cfg!(feature = "lenient") {
                        //Just use the remainder of the file
                        mem::replace(raw, &[])
                    } else {
                        bail!(err_malformed("reached eof before chunk ended"));
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

    /// Interpret the chunk as a track.
    fn parse_into_track<T: TrackRepr<'a>>(
        chunk_parse_result: Result<Chunk<'a>>,
    ) -> Option<Result<T>> {
        match chunk_parse_result {
            Ok(Chunk::Track(track)) => Some(T::read(track)),
            //Read another header (?)
            Ok(Chunk::Header(..)) => {
                if cfg!(feature = "lenient") {
                    //Ignore duplicate header
                    None
                } else {
                    Some(Err(err_malformed("found duplicate header").into()))
                }
            }
            //Failed to read chunk
            Err(err) => {
                if cfg!(feature = "lenient") {
                    //Ignore invalid chunk
                    None
                } else {
                    Some(
                        Err(err)
                            .context(err_malformed("invalid chunk"))
                            .map_err(|err| err.into()),
                    )
                }
            }
        }
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

/// Allows for customization on how tracks are stored in memory.
pub trait TrackRepr<'a>: Sized + Send {
    const USE_MULTITHREADING: bool;
    fn read(data: &'a [u8]) -> Result<Self>;
    #[cfg(feature = "std")]
    fn write<W: Write>(&self, out: &mut W) -> IoResult<()>;
}

/// Allows deferring track parsing for later, on a per-event basis.
///
/// This is the best option when traversing a track once or for saving memory.
/// This `struct` is very light, so it can be copied freely.
#[derive(Clone, Debug)]
pub struct TrackIter<'a> {
    raw: &'a [u8],
    running_status: Option<u8>,
}
impl<'a> TrackRepr<'a> for TrackIter<'a> {
    const USE_MULTITHREADING: bool = false;
    fn read(raw: &'a [u8]) -> Result<TrackIter<'a>> {
        Ok(TrackIter {
            raw,
            running_status: None,
        })
    }
    #[cfg(feature = "std")]
    fn write<W: Write>(&self, out: &mut W) -> IoResult<()> {
        out.write_all(self.raw)?;
        Ok(())
    }
}
impl<'a> Iterator for TrackIter<'a> {
    type Item = Result<(&'a [u8], Event<'a>)>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.raw.len() > 0 {
            let read_result = Event::read(&mut self.raw, &mut self.running_status);
            if cfg!(feature = "lenient") {
                match read_result {
                    Ok(ev) => Some(Ok(ev)),
                    //Ignore errors
                    Err(_err) => None,
                }
            } else {
                Some(read_result.context(err_malformed("malformed event")))
            }
        } else {
            None
        }
    }
}

/// Allows processing the entire track at once and storing the parsed events into a vector.
/// Parses events only once, but allocates a "large" amount of memory.
impl<'a> TrackRepr<'a> for Vec<(&'a [u8], Event<'a>)> {
    const USE_MULTITHREADING: bool = true;
    fn read(raw: &'a [u8]) -> Result<Self> {
        TrackIter::read(raw)?.collect::<Result<Vec<_>>>()
    }
    #[cfg(feature = "std")]
    fn write<W: Write>(&self, out: &mut W) -> IoResult<()> {
        let mut running_status = None;
        for (_data, event) in self.iter() {
            event.write(&mut running_status, out)?;
        }
        Ok(())
    }
}
/// Similar to `Vec<(&[u8],Event)>`, but throws away the bytes associated with each event to save
/// memory.
impl<'a> TrackRepr<'a> for Vec<Event<'a>> {
    const USE_MULTITHREADING: bool = true;
    fn read(raw: &'a [u8]) -> Result<Self> {
        TrackIter::read(raw)?
            .map(|res| res.map(|(_, ev)| ev))
            .collect::<Result<Vec<_>>>()
    }
    #[cfg(feature = "std")]
    fn write<W: Write>(&self, out: &mut W) -> IoResult<()> {
        let mut running_status = None;
        for event in self.iter() {
            event.write(&mut running_status, out)?;
        }
        Ok(())
    }
}
