use crate::{event::Event,prelude::*,primitive::{Format, Timing}};
use rayon;
use std::{fs::File,io::{Read, self},path::Path};

/// "Easy" way to do file loading.
#[derive(Clone, Debug)]
pub struct SmfBuffer(Box<[u8]>);
impl SmfBuffer {
    /// Creates an SmfBuffer from a normal buffer.
    pub fn new<B: Into<Box<[u8]>>>(buf: B) -> SmfBuffer {
        SmfBuffer(buf.into())
    }
    /// Opens and reads a file, storing it in memory but not parsing it.
    pub fn open<P: AsRef<Path>>(path: P) -> io::Result<SmfBuffer> {
        let mut file = File::open(path)?;
        let mut buffer =
            Vec::with_capacity((file.metadata().map(|meta| meta.len()).unwrap_or(0) + 1) as usize);
        file.read_to_end(&mut buffer)?;
        Ok(SmfBuffer::new(buffer))
    }
    /// Borrows the full in-memory buffer and parses an SMF generically.
    /// Due to generics, the `parse_*` functions are preferred.
    /// In doubt, use `parse_collect`, which automatically does multithreaded parsing and stores
    /// events in an easy-to-use `Vec`.
    pub fn parse<'a, T: TrackRepr<'a>>(&'a self) -> Result<Smf<'a, T>> {
        Smf::read(&self.0)
    }
    /// Parses an SMF but not its events, deferring it for later.
    /// The iterator will parse lazily, meaning it will do no allocations. This means performance
    /// gains if you know what you're doing and implement multithreading yourself.
    /// Yields both underlying bytes and events.
    pub fn parse_defer<'a>(&'a self) -> Result<Smf<TrackIter<'a>>> {
        self.parse()
    }
    /// Parses an SMF, collecting into a `Vec` only events, discarding raw bytes to save memory.
    pub fn parse_collect<'a>(&'a self) -> Result<Smf<Vec<Event<'a>>>> {
        self.parse()
    }
    /// Parses an SMF, collecting events and their underlying bytes into a `Vec`.
    pub fn parse_collect_bytes<'a>(&'a self) -> Result<Smf<Vec<(&'a [u8], Event<'a>)>>> {
        self.parse()
    }
}

/// Represents a Standard Midi File (.mid and .midi files).
/// Yields `TrackRepr` on a Vec, allowing for customization on what is stored in a track.
/// References an outside byte array.
#[derive(Clone, Debug)]
pub struct Smf<'a, T: TrackRepr<'a>> {
    pub header: Header,
    pub tracks: Vec<T>,
    _lifetime: PhantomData<&'a ()>,
}
impl<'a, T: TrackRepr<'a>> Smf<'a, T> {
    pub fn new(header: Header, tracks: Vec<T>) -> Result<Smf<'a, T>> {
        if header.track_count as usize != tracks.len() {
            bail!("file has a different amount of tracks than declared");
        }
        Ok(Smf {
            header,
            tracks,
            _lifetime: PhantomData,
        })
    }
    pub fn read(raw: &'a [u8]) -> Result<Smf<'a, T>> {
        let mut chunks = ChunkIter::read(raw);
        let header = match chunks.next().ok_or(err_msg("expected header, found eof"))?? {
            Chunk::Header(header) => Ok(header),
            Chunk::Track(_) => Err(err_msg("midi header not found")),
        }?;
        let mut tracks: Vec<Result<_>> = Vec::with_capacity(header.track_count as usize);
        tracks.extend(
            (0..header.track_count)
                .map(|_| Err(err_msg("less tracks found than declared in header").into())),
        );
        rayon::scope(|s| {
            for (chunk, track) in chunks.zip(tracks.iter_mut()) {
                s.spawn(move |_| {
                    *track = (|| match chunk? {
                        Chunk::Track(track) => T::read(track),
                        Chunk::Header(_) => Err(err_msg("found duplicate header").into()),
                    })()
                });
            }
        });
        let tracks = tracks.into_iter().collect::<Result<Vec<T>>>()?;
        /*
        chunks.par_iter().map(|result| result.and_then(|chunk| {
          match chunk {
            Chunk::Track(track)=>T::read(track),
            Chunk::Header(_)=>Err("found duplicate header".into()),
          }
        })).collect::<Result<Vec<_>>>()?;

        */
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
        if raw.len() == 0 {
            return Ok(None);
        }
        let id = raw
            .split_checked(4)
            .ok_or(err_msg("failed to read expected chunkid"))?;
        let len = u32::read(raw).context("failed to read expected chunklen")?;
        let chunkdata = raw
            .split_checked(len as usize)
            .ok_or(err_msg("reached eof before chunk ended"))?;
        if id == "MThd".as_bytes() {
            Ok(Some(Chunk::Header(Header::read(chunkdata)?)))
        } else if id == "MTrk".as_bytes() {
            Ok(Some(Chunk::Track(chunkdata)))
        } else {
            //Unknown chunk, just ignore and read the next one (notice tail call)
            Chunk::read(raw)
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
        if let Format::SingleTrack = format {
            if track_count != 1 {
                bail!("singletrack format file has multiple tracks")
            }
        }
        let timing = Timing::read(&mut raw)?;
        Ok(Header::new(format, timing, track_count))
    }
}

/// Allows for customization on how tracks are stored in memory.
/// Check implementors for options.
pub trait TrackRepr<'a>: Sized + Send {
    fn read(data: &'a [u8]) -> Result<Self>;
}

/// Allows deferring track parsing for later, on a per-event basis.
/// This is the best option when traversing a track once or for saving memory.
/// This `struct` is very light, it can be copied freely.
#[derive(Clone, Debug)]
pub struct TrackIter<'a> {
    raw: &'a [u8],
    running_status: Option<u8>,
}
impl<'a> TrackRepr<'a> for TrackIter<'a> {
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
            Some(Event::read(&mut self.raw, &mut self.running_status))
        } else {
            None
        }
    }
}

/// Allows processing the entire track at once and storing the parsed events into a vector.
/// Parses events only once, but allocates a "large" amount of memory.
impl<'a> TrackRepr<'a> for Vec<(&'a [u8], Event<'a>)> {
    fn read(raw: &'a [u8]) -> Result<Self> {
        TrackIter::read(raw)?.collect::<Result<Vec<_>>>()
    }
}
/// Similar to `Vec<(&[u8],Event)>`, but throws away the bytes associated with each event to save
/// memory.
impl<'a> TrackRepr<'a> for Vec<Event<'a>> {
    fn read(raw: &'a [u8]) -> Result<Self> {
        TrackIter::read(raw)?
            .map(|res| res.map(|(_, ev)| ev))
            .collect::<Result<Vec<_>>>()
    }
}
