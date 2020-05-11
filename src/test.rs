use crate::{Event, EventIter, Result as MidlyResult};
use std::{fs, path::Path, time::Instant};

/// Open and read the content of a file.
macro_rules! open {
    {$name:ident : $file:expr} => {
        let $name = fs::read(AsRef::<Path>::as_ref("test-asset").join($file)).unwrap();
    };
    {$name:ident : [$parse:ident] $file:expr} => {
        let $name = match $parse::Smf::parse(&$file[..]) {
            Ok(smf) => smf,
            Err(err) => {
                eprintln!("failed to parse test file: {:?}", err);
                panic!()
            },
        };
    };
}

/// Macro for parsing a MIDI file.
macro_rules! test {
    {$file:expr => $parse_method:ident} => {{
        let counts = time(&$file.to_string(), ||->Vec<_> {
            open!{file: $file};
            open!{smf: [$parse_method] file};
            smf.tracks.into_iter().map(|track| $parse_method::len(&file[..], track)).collect()
        });
        for (i,count) in counts.iter().enumerate() {
            println!("track {} has {} events", i, count);
        }
    }};
}

#[cfg(not(feature = "alloc"))]
impl crate::io::Write for Vec<u8> {
    type Error = &'static str;
    type Seekable = crate::io::NotSeekable<Self>;
    fn write_all(&mut self, buf: &[u8]) -> Result<(), &'static str> {
        self.extend_from_slice(buf);
        Ok(())
    }
    fn invalid_input(msg: &'static str) -> &'static str {
        msg
    }
}

fn test_rewrite(filename: &str) {
    println!("parsing...");
    open! {smf: filename};
    open! {smf: [parse_collect] smf};
    println!("rewriting...");
    let mut file = Vec::with_capacity(16 * 1024);
    time(&format!("{} (rewrite)", filename), || {
        smf.write(&mut file).expect("failed to rewrite midi file");
    });
    println!("reparsing...");
    let clone_smf = time(&format!("{} (reparse)", filename), || {
        parse_collect::Smf::parse(&file).expect("failed to reparse midi file")
    });
    assert_eq!(
        smf, clone_smf,
        "reparsed midi file is not identical to the original"
    );
}

mod parse_collect {
    use super::*;
    #[cfg(feature = "alloc")]
    pub use crate::Smf;
    #[cfg(not(feature = "alloc"))]
    #[derive(Clone, PartialEq, Eq, Debug)]
    pub struct Smf<'a> {
        pub header: crate::Header,
        pub tracks: Vec<Vec<Event<'a>>>,
    }
    #[cfg(not(feature = "alloc"))]
    impl<'a> Smf<'a> {
        pub fn parse(raw: &[u8]) -> MidlyResult<Smf> {
            let (header, tracks) = crate::parse(raw)?;
            Ok(Smf {
                header,
                tracks: tracks
                    .map(|events| events.and_then(|evs| evs.collect::<MidlyResult<Vec<_>>>()))
                    .collect::<MidlyResult<Vec<_>>>()?,
            })
        }
        pub fn write<W: crate::io::Write>(&self, out: &mut W) -> Result<(), W::Error> {
            crate::write(
                &self.header,
                self.tracks.len(),
                |idx| self.tracks[idx].iter(),
                out,
            )
        }
    }
    pub fn len(_raw: &[u8], track: Vec<Event>) -> usize {
        track.len()
    }
}
mod parse_bytemap {
    use super::*;
    #[cfg(feature = "alloc")]
    pub use crate::SmfBytemap as Smf;
    #[cfg(not(feature = "alloc"))]
    #[derive(Clone, PartialEq, Eq, Debug)]
    pub struct Smf<'a> {
        pub header: crate::Header,
        pub tracks: Vec<Vec<(&'a [u8], Event<'a>)>>,
    }
    #[cfg(not(feature = "alloc"))]
    impl<'a> Smf<'a> {
        pub fn parse(raw: &[u8]) -> MidlyResult<Smf> {
            let (header, tracks) = crate::parse(raw)?;
            Ok(Smf {
                header,
                tracks: tracks
                    .map(|events| {
                        events.and_then(|evs| evs.bytemapped().collect::<MidlyResult<Vec<_>>>())
                    })
                    .collect::<MidlyResult<Vec<_>>>()?,
            })
        }
    }
    pub fn len(mut raw: &[u8], track: Vec<(&[u8], Event)>) -> usize {
        //Quick and dirty test to make sure events bytes are present in the source in order, and
        //NOT consecutive (because delta times must interrupt every single event)
        for (bytes, _ev) in track.iter() {
            let mut advanced = false;
            while !raw.starts_with(*bytes) {
                advanced = true;
                match raw.get(1..) {
                    Some(new_raw) => raw = new_raw,
                    None => panic!("event bytes are not present in the raw bytes"),
                }
            }
            assert!(advanced, "event bytes cannot be consecutive");
            raw = &raw[bytes.len()..];
        }
        track.len()
    }
}
mod parse_lazy {
    use super::*;
    pub struct Smf<'a> {
        pub header: crate::Header,
        pub tracks: crate::TrackIter<'a>,
    }
    impl Smf<'_> {
        pub fn parse(raw: &[u8]) -> MidlyResult<Smf> {
            let (header, tracks) = crate::parse(raw)?;
            Ok(Smf { header, tracks })
        }
    }
    pub fn len(_raw: &[u8], track: MidlyResult<EventIter>) -> usize {
        match track {
            Ok(track) => track.count(),
            Err(err) => {
                println!("failed to parse track: {}", err);
                0
            }
        }
    }
}

/// Take note of how long it takes to parse.
fn time<F: FnOnce() -> R, R>(activity: &str, op: F) -> R {
    let start = Instant::now();
    let result = op();
    let took = Instant::now() - start;
    println!("{}: {}ms", activity, (took * 1000).as_secs());
    result
}

fn test_event_api(file: &str) {
    open! {file: file};
    open! {smf: [parse_bytemap] file};
    for (bytes, ev) in smf.tracks.iter().flat_map(|track| track.iter()) {
        use crate::{num::u7, EventKind, StreamEvent, SystemCommon};
        match ev.kind {
            EventKind::Midi { channel, message } => {
                let mut raw_bytes;
                let stream_ev = if bytes.first().map(|&b| b < 0x80).unwrap_or(true) {
                    raw_bytes = vec![message.status_nibble() << 4 | channel.as_int()];
                    raw_bytes.extend_from_slice(bytes);
                    StreamEvent::parse(&raw_bytes[..]).unwrap()
                } else {
                    StreamEvent::parse(bytes).unwrap()
                };
                assert_eq!(stream_ev, StreamEvent::Midi { channel, message });
            }
            EventKind::SysEx(sysex_bytes) => {
                assert!(
                    sysex_bytes.last() == Some(&0xF7),
                    "cannot test fragmented sysex with event api"
                );
                assert!(
                    sysex_bytes[..sysex_bytes.len() - 1]
                        .iter()
                        .all(|&b| b < 0x80),
                    "sysex byte with top bit set"
                );
                let mut raw_bytes = vec![0xF0];
                raw_bytes.extend_from_slice(sysex_bytes);
                let stream_ev = StreamEvent::parse(&raw_bytes).unwrap();
                assert_eq!(
                    stream_ev,
                    StreamEvent::Common(SystemCommon::SysEx(u7::from_int_slice(sysex_bytes)))
                );
            }
            EventKind::Escape(_) => {
                panic!("cannot test arbitrary escaped with event api");
            }
            EventKind::Meta(_) => {}
        }
    }
}

fn test_stream_api(file: &str) {
    use crate::{num::u7, EventKind, MidiStream, StreamEvent, SystemCommon, SystemRealtime};

    #[derive(Debug)]
    struct EventData<'a> {
        fired_at: usize,
        event: Result<StreamEvent<'a>, (usize, usize)>,
    }

    open! {file: file};
    open! {smf: [parse_bytemap] file};
    //Holds data bytes for sysex expected events
    let mut sysex_bytes = Vec::new();
    //Holds expected events
    let mut expected_evs = Vec::new();
    //Holds the raw reconstructed bytes to be re-parsed
    let mut byte_stream = Vec::new();
    for (bytes, ev) in smf.tracks.iter().flat_map(|track| track.iter()) {
        match ev.kind {
            EventKind::Midi { channel, message } => {
                //Write down the status byte if missing
                if bytes[0] < 0x80 {
                    byte_stream.push(message.status_nibble() << 4 | channel.as_int());
                }
                //Write down the message bytes, directly from the source bytes
                byte_stream.extend_from_slice(bytes);
                //Add an expected event
                expected_evs.push(EventData {
                    fired_at: byte_stream.len(),
                    event: Ok(StreamEvent::Midi { channel, message }),
                });
            }
            EventKind::SysEx(data) => {
                assert!(
                    data.iter()
                        .enumerate()
                        .all(|(i, &b)| (i == data.len() - 1 && b == 0xF7) || b < 0x80),
                    "sysex byte with top bit set"
                );
                byte_stream.push(0xF0);
                byte_stream.extend_from_slice(data);
                let data_start = sysex_bytes.len();
                sysex_bytes.extend_from_slice(data);
                let data_end = sysex_bytes.len();
                expected_evs.push(EventData {
                    fired_at: byte_stream.len(),
                    event: Err((data_start, data_end)),
                });
            }
            EventKind::Escape(data) => {
                if let Some(EventData {
                    fired_at,
                    event: Err((_data_start, data_end)),
                }) = expected_evs.last_mut()
                {
                    assert!(
                        sysex_bytes.last() != Some(&0xF7),
                        "escape cannot continue finished sysex"
                    );
                    assert!(
                        data.iter()
                            .enumerate()
                            .all(|(i, &b)| (i == data.len() - 1 && b == 0xF7) || b < 0x80),
                        "sysex byte with top bit set"
                    );
                    sysex_bytes.extend_from_slice(data);
                    byte_stream.extend_from_slice(data);
                    *data_end = sysex_bytes.len();
                    *fired_at = byte_stream.len();
                } else {
                    panic!("cannot test arbitrary escape events");
                }
            }
            EventKind::Meta(_) => {}
        }
    }
    //Sprinkle bytestream with system realtime events
    {
        let mut next_idx = 3;
        let mut next_byte = 0xFF;
        let mut stride = 1;
        let mut event_idx = 0;
        let mut inserted_bytes = 0;
        let orig_len = byte_stream.len();
        while next_idx <= orig_len {
            while expected_evs
                .get(event_idx)
                .map(|ev| ev.fired_at < next_idx)
                .unwrap_or(false)
            {
                event_idx += 1;
            }
            byte_stream.insert(next_idx + inserted_bytes, next_byte);
            inserted_bytes += 1;
            expected_evs.insert(
                event_idx,
                EventData {
                    fired_at: byte_stream.len() - 1,
                    event: Ok(StreamEvent::Realtime(SystemRealtime::read(next_byte))),
                },
            );
            event_idx += 1;
            //Advance pseudorandomly
            next_idx += stride;
            stride = (stride + 5) % 8;
            next_byte = (next_byte - 0xF8 + 3) % 8 + 0xF8;
        }
    }
    //Resolve sysex byte indices to actual byte slices
    let mut expected_evs = expected_evs.into_iter().map(|ev| match ev {
        EventData { event: Ok(ev), .. } => ev,
        EventData {
            event: Err((data_start, data_end)),
            ..
        } => StreamEvent::Common(SystemCommon::SysEx(u7::from_int_slice(
            &sysex_bytes[data_start..data_end],
        ))),
    });
    //Parse the bytestream, comparing any events that fire to the expected events list
    let mut stream = MidiStream::new();
    let mut handle_ev = |ev: StreamEvent| {
        let expected = expected_evs.next().expect("stream produced excess events");
        assert_eq!(ev, expected);
    };
    let mut chunk_size = 1;
    let mut byte_stream = &byte_stream[..];
    while !byte_stream.is_empty() {
        //Take a pseudo-randomly sized chunk and feed it to the stream parser
        use crate::primitive::SplitChecked;
        chunk_size = chunk_size % 5 + 1;
        let chunk = match byte_stream.split_checked(chunk_size) {
            Some(chunk) => chunk,
            None => {
                let chunk = byte_stream;
                byte_stream = &[];
                chunk
            }
        };
        stream.feed(chunk, &mut handle_ev);
    }
    //Flush any pending event
    stream.flush(handle_ev);
    //Make sure all expected events were fired
    assert_eq!(
        expected_evs.next(),
        None,
        "stream produced too little events"
    );
}

macro_rules! def_tests {
    ($(
        $(#[$attr:meta])*
        fn $testname:ident() { $filename:expr }
    )*) => {$(
        mod $testname {
            use super::*;

            $(#[$attr])*
            fn parse_lazy() {
                test!($filename => parse_lazy);
            }
            $(#[$attr])*
            fn parse() {
                test!($filename => parse_collect);
            }
            $(#[$attr])*
            fn parse_bytemap() {
                test!($filename => parse_bytemap);
            }
            $(#[$attr])*
            fn event_api() {
                test_event_api($filename);
            }
            $(#[$attr])*
            fn stream_api() {
                test_stream_api($filename);
            }
            $(#[$attr])*
            fn rewrite() {
                test_rewrite($filename);
            }
        }
    )*}
}

/// Test the MIDI parser on several files.
mod parse {
    use super::*;

    def_tests! {
        #[test]
        fn clementi() {"Clementi.mid"}

        #[test]
        fn sandstorm() {"Sandstorm.mid"}

        #[test]
        #[cfg_attr(feature = "strict", should_panic)]
        fn pidamaged() {"PiDamaged.mid"}

        #[test]
        fn levels() {"Levels.mid"}

        #[test]
        fn beethoven() {"Beethoven.rmi"}

        #[test]
        fn sysex() {"SysExTest.mid"}
    }

    #[test]
    fn not_midi() {
        open! {file: "colorlist.txt"};
        let result = parse_collect::Smf::parse(&file);
        match result {
            Ok(_) => panic!("parsed an invalid midi file"),
            Err(err) => match err.kind() {
                crate::ErrorKind::Invalid(_) => {}
                crate::ErrorKind::Malformed(_) => {
                    panic!("invalid midi file produced a malformed (not invalid) errorkind")
                }
            },
        }
    }
}
