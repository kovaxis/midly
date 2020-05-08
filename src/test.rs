use crate::{Event, EventIter, Result as MidlyResult};
use std::{fs, time::Instant};

/// Open and read the content of a file.
macro_rules! open {
    {$name:ident : $file:expr} => {
        let $name = fs::read(concat!("test-asset/", $file)).unwrap();
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
    {($name:expr , $file:expr) => $parse_method:ident} => {{
        let counts = time($name, ||->Vec<_> {
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

macro_rules! test_rewrite {
    ($name:expr, $file:expr) => {{
        println!("parsing...");
        open! {smf: $file};
        open! {smf: [parse_collect] smf};
        println!("rewriting...");
        let mut file = Vec::with_capacity(16 * 1024);
        time(concat!($name, "[rewrite]"), || {
            smf.write(&mut file).expect("failed to rewrite midi file");
        });
        println!("reparsing...");
        let clone_smf = time(concat!($name, "[reparse]"), || {
            parse_collect::Smf::parse(&file).expect("failed to reparse midi file")
        });
        assert_eq!(
            smf, clone_smf,
            "reparsed midi file is not identical to the original"
        );
    }};
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

/// Test the MIDI parser on several files.
mod parse {
    use super::*;

    #[test]
    fn clementi_lazy() {
        test!(("parse_clementi_lazy","Clementi.mid") => parse_lazy);
    }

    #[test]
    fn clementi_bytemap() {
        test!(("parse_clementi_bytemap", "Clementi.mid") => parse_bytemap);
    }

    #[test]
    fn clementi() {
        test!(("parse_clementi","Clementi.mid") => parse_collect);
    }

    #[test]
    fn sandstorm() {
        test!(("parse_sandstorm","Sandstorm.mid") => parse_collect);
    }

    #[test]
    fn sandstorm_bytemap() {
        test!(("parse_sandstorm_bytemap", "Sandstorm.mid") => parse_bytemap);
    }

    #[test]
    #[cfg_attr(feature = "strict", should_panic)]
    fn pidamaged() {
        test!(("parse_pidamaged","PiDamaged.mid") => parse_collect);
    }

    #[test]
    #[cfg_attr(feature = "strict", should_panic)]
    fn pidamaged_bytemap() {
        test!(("parse_pidamaged_bytemap", "PiDamaged.mid") => parse_bytemap);
    }

    #[test]
    fn levels() {
        test!(("parse_levels","Levels.mid") => parse_collect);
    }

    #[test]
    fn beethoven() {
        test!(("parse_beethoven","Beethoven.rmi") => parse_collect);
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

/// Test the MIDI writer and parser.
mod write {
    use super::*;

    #[test]
    fn clementi_rewrite() {
        test_rewrite!("clementi_rewrite", "Clementi.mid");
    }

    #[test]
    fn sandstorm_rewrite() {
        test_rewrite!("sandstorm_rewrite", "Sandstorm.mid");
    }

    #[test]
    #[cfg_attr(feature = "strict", should_panic)]
    fn pidamaged_rewrite() {
        test_rewrite!("pidamaged_rewrite", "PiDamaged.mid");
    }

    #[test]
    fn levels_rewrite() {
        test_rewrite!("levels_rewrite", "Levels.mid");
    }
}
