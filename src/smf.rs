use crate::{
    event::Event,
    prelude::*,
    primitive::{Format, Timing},
};

/// Represents a Standard Midi File (.mid and .midi files).
/// Yields `TrackRepr` on a Vec, allowing for customization on what is stored in a track.
/// References an outside byte array.
#[derive(Clone, Debug)]
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
        if !cfg!(feature = "lenient") {
            ensure!(
                header.track_count as usize == tracks.len(),
                err_malformed("file has a different amount of tracks than declared")
            );
        }
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
        let header = match chunks.next() {
            Some(maybe_chunk) => match maybe_chunk.context(err_invalid("invalid midi header"))? {
                Chunk::Header(header) => Ok(header),
                Chunk::Track(_) => Err(err_invalid("expected header, found track")),
            },
            None => Err(err_invalid("empty file")),
        }?;
        let tracks = chunks.parse_as_tracks(header)?;
        Ok(Smf::new(header, tracks)?)
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
    fn parse_as_tracks<T: TrackRepr<'a>>(self, header: Header) -> Result<Vec<T>> {
        //Attempt to use multiple threads if possible and enabled
        #[cfg(feature = "std")]
        {
            if T::USE_MULTITHREADING {
                use rayon::prelude::*;

                let mut chunk_vec = Vec::with_capacity(header.track_count as usize);
                chunk_vec.extend(self);
                return chunk_vec
                    .into_par_iter()
                    .filter_map(Chunk::parse_into_track)
                    .collect::<Result<Vec<T>>>();
            }
        }
        //Fall back to single-threaded
        let mut tracks = Vec::with_capacity(header.track_count as usize);
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
    Header(Header),
    Track(&'a [u8]),
}
impl<'a> Chunk<'a> {
    /// Should be called with a byte slice at least as large as the chunk (ideally until EOF).
    /// The slice will be modified to point to the next chunk.
    /// If we're *exactly* at EOF (slice length 0), returns a None signalling no more chunks.
    fn read(raw: &mut &'a [u8]) -> Result<Option<Chunk<'a>>> {
        loop {
            if raw.len() == 0 {
                break Ok(None);
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
            if id == "MThd".as_bytes() {
                break Ok(Some(Chunk::Header(Header::read(chunkdata)?)));
            } else if id == "MTrk".as_bytes() {
                break Ok(Some(Chunk::Track(chunkdata)));
            } else {
                //Unknown chunk, just ignore and read the next one
            }
        }
    }

    /// Interpret the chunk as a track.
    fn parse_into_track<T: TrackRepr<'a>>(
        chunk_parse_result: Result<Chunk<'a>>,
    ) -> Option<Result<T>> {
        match chunk_parse_result {
            Ok(Chunk::Track(track)) => Some(T::read(track)),
            //Read another header (?)
            Ok(Chunk::Header(_)) => {
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
#[derive(Copy, Clone, Debug)]
pub struct Header {
    pub format: Format,
    pub timing: Timing,
    track_count: u16,
}
impl Header {
    pub fn new(format: Format, timing: Timing, track_count: u16) -> Header {
        Header {
            format,
            timing,
            track_count,
        }
    }

    pub fn read(mut raw: &[u8]) -> Result<Header> {
        let format = Format::read(&mut raw)?;
        let track_count = u16::read(&mut raw)?;
        if !cfg!(feature = "lenient") {
            if let Format::SingleTrack = format {
                ensure!(
                    track_count == 1,
                    err_malformed("singletrack format file has multiple tracks")
                );
            }
        }
        let timing = Timing::read(&mut raw)?;
        Ok(Header::new(format, timing, track_count))
    }
}

/// Allows for customization on how tracks are stored in memory.
pub trait TrackRepr<'a>: Sized + Send {
    const USE_MULTITHREADING: bool;
    fn read(data: &'a [u8]) -> Result<Self>;
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
}
