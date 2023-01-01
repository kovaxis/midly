use crate::{iter::EventIter, Result as MidlyResult};
use midly_core::MidiMessage;
use std::{fs, path::Path, time::Instant};

/// Open and read the content of a file.
macro_rules! open {
    {$name:ident : $file:expr} => {
        let mut $name = fs::read(AsRef::<Path>::as_ref("test-asset").join($file)).unwrap();
    };
    {$name:ident : [$parse:ident] $file:expr} => {
        let $name = match $parse::Smf::parse(&mut $file[..]) {
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
            smf.tracks.into_iter().map(|track| $parse_method::len(track)).collect()
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
    fn write(&mut self, buf: &[u8]) -> Result<(), &'static str> {
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
        parse_collect::Smf::parse(&mut file).expect("failed to reparse midi file")
    });
    assert_eq!(
        smf, clone_smf,
        "reparsed midi file is not identical to the original"
    );
}

mod parse_collect {
    #[cfg(not(feature = "alloc"))]
    use super::*;
    #[cfg(not(feature = "alloc"))]
    use crate::TrackEvent;
    #[cfg(feature = "alloc")]
    pub use crate::{Smf, Track};
    #[cfg(not(feature = "alloc"))]
    #[derive(Clone, PartialEq, Eq, Debug)]
    pub struct Smf<'a> {
        pub header: crate::Header,
        pub tracks: Vec<Track<'a>>,
    }
    #[cfg(not(feature = "alloc"))]
    #[derive(Clone, PartialEq, Eq, Debug)]
    pub struct Track<'a> {
        pub events: Vec<TrackEvent<'a>>,
    }
    #[cfg(not(feature = "alloc"))]
    impl<'a> Smf<'a> {
        pub fn parse(raw: &mut [u8]) -> MidlyResult<Smf> {
            let (header, tracks) = crate::parse(raw)?;
            Ok(Smf {
                header,
                tracks: tracks
                    .map(|events| {
                        events
                            .and_then(|evs| evs.collect::<MidlyResult<Vec<_>>>())
                            .map(|evs| Track { events: evs })
                    })
                    .collect::<MidlyResult<Vec<Track>>>()?,
            })
        }
        pub fn write<W: crate::io::Write>(&self, out: &mut W) -> Result<(), W::Error> {
            crate::write(
                &self.header,
                self.tracks.iter().map(|track| &track.events),
                out,
            )
        }
    }
    pub fn len(track: Track) -> usize {
        track.events.len()
    }
}
mod parse_lazy {
    use super::*;
    pub struct Smf<'a> {
        pub header: crate::Header,
        pub tracks: crate::iter::TrackIter<'a>,
    }
    impl Smf<'_> {
        pub fn parse(raw: &mut [u8]) -> MidlyResult<Smf> {
            let (header, tracks) = crate::parse(raw)?;
            Ok(Smf { header, tracks })
        }
    }
    pub fn len(track: MidlyResult<EventIter>) -> usize {
        match track {
            Ok(track) => track.count(),
            Err(err) => panic!("failed to parse track: {}", err),
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

fn test_live_api(file: &str) {
    open! {file: file};
    open! {smf: [parse_collect] file};
    for ev in smf.tracks.iter().flat_map(|track| track.events.iter()) {
        if ev.msg.is_meta() {
            continue;
        }
        let mut buf = [0; 3];
        let bytes = ev.msg.encode(&mut buf);
        let backmsg = MidiMessage::decode(bytes);
        assert_eq!(ev.msg, backmsg);
    }
}

fn test_stream_api(file: &str) {
    use midly_rawstream::MidiStream;

    #[derive(Debug)]
    struct EventData<'a> {
        fired_at: usize,
        event: Result<MidiMessage<'a>, (usize, usize)>,
    }

    open! {file: file};
    open! {smf: [parse_collect] file};
    //Holds data bytes for sysex expected events
    let mut sysex_bytes = Vec::new();
    //Holds expected events
    let mut expected_evs = Vec::new();
    //Holds the raw reconstructed bytes to be re-parsed
    let mut byte_stream = Vec::new();
    for ev in smf.tracks.iter().flat_map(|track| track.events.iter()) {
        match &ev.msg {
            MidiMessage::SysEx(data) => {
                assert_eq!(data.first(), Some(&0xF0));
                assert!(
                    data.iter()
                        .enumerate()
                        .skip(1)
                        .all(|(i, &b)| (i == data.len() - 1 && b == 0xF7) || b < 0x80),
                    "sysex byte with top bit set: {:x?}",
                    data
                );
                byte_stream.extend_from_slice(data);
                let data_start = sysex_bytes.len();
                sysex_bytes.extend_from_slice(data);
                let data_end = sysex_bytes.len();
                expected_evs.push(EventData {
                    fired_at: byte_stream.len(),
                    event: Err((data_start, data_end)),
                });
            }
            MidiMessage::Unspecified(data)
                if !data.get(0).map(|&st| st >= 0x80).unwrap_or(false) =>
            {
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
            _ if !ev.msg.is_meta() => {
                //Write down the message bytes
                let mut buf = [0; 3];
                byte_stream.extend_from_slice(ev.msg.encode(&mut buf));
                //Add an expected event
                expected_evs.push(EventData {
                    //Midi messages are fired as soon as the last data byte arrives, therefore the
                    //length - 1
                    fired_at: byte_stream.len() - if ev.msg.is_channel() { 1 } else { 0 },
                    event: Ok(ev.msg.clone()),
                });
            }
            _ => {}
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
                    event: Ok(MidiMessage::decode(&[next_byte]).into_owned()),
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
    let expected_evs = expected_evs
        .into_iter()
        .map(|ev| match ev {
            EventData { event: Ok(ev), .. } => ev,
            EventData {
                event: Err((data_start, data_end)),
                ..
            } => MidiMessage::SysEx(sysex_bytes[data_start..data_end].into()),
        })
        .collect::<Vec<_>>();
    println!("bytestream: {:x?}", byte_stream);
    println!("expected: {:?}", expected_evs);
    let mut expected_evs = expected_evs.into_iter();
    //Parse the bytestream, comparing any events that fire to the expected events list
    let mut stream = MidiStream::new();
    let mut handle_ev = |ev: MidiMessage| {
        let expected = expected_evs.next().expect("stream produced excess events");
        assert_eq!(ev, expected, "reparsed != expected");
        println!("correctly received {:?}", ev);
    };
    let mut chunk_size = 1;
    let mut byte_stream = &byte_stream[..];
    while !byte_stream.is_empty() {
        //Take a pseudo-randomly sized chunk and feed it to the stream parser
        chunk_size = chunk_size % 5 + 1;
        let (chunk, rem) = byte_stream.split_at(chunk_size.min(byte_stream.len()));
        byte_stream = rem;
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
            fn live_api() {
                test_live_api($filename);
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
        let result = parse_collect::Smf::parse(&mut file);
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
