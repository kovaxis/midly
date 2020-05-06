use crate::{Event, EventIter, Result as MidlyResult, Smf};
use failure::Fail;
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
                eprintln!("failed to parse test file:");
                for cause in (&err as &dyn Fail).iter_chain() {
                    eprintln!("  {}", cause);
                }
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
            Smf::parse(&file).expect("failed to reparse midi file")
        });
        assert_eq!(
            smf, clone_smf,
            "reparsed midi file is not identical to the original"
        );
    }};
}

mod parse_collect {
    use super::*;
    pub use crate::Smf;
    pub fn len(_raw: &[u8], track: Vec<Event>) -> usize {
        track.len()
    }
}
mod parse_bytemap {
    use super::*;
    pub use crate::SmfBytemap as Smf;
    pub fn len(mut raw: &[u8], track: Vec<(&[u8], Event)>) -> usize {
        //Make sure the bytes of each event are actually present in the source
        for (bytes, _ev) in track.iter() {
            while !raw.starts_with(*bytes) {
                match raw.get(1..) {
                    Some(new_raw) => raw = new_raw,
                    None => panic!("event bytes are not present in the raw bytes"),
                }
            }
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
        test!(("parse_clementi_vec","Clementi.mid") => parse_collect);
    }

    #[test]
    fn sandstorm() {
        test!(("parse_sandstorm_vec","Sandstorm.mid") => parse_collect);
    }

    #[test]
    #[cfg_attr(feature = "strict", should_panic)]
    fn pidamaged() {
        test!(("parse_pidamaged_vec","PiDamaged.mid") => parse_collect);
    }

    #[test]
    fn levels() {
        test!(("parse_levels_vec","Levels.mid") => parse_collect);
    }

    #[test]
    fn beethoven() {
        test!(("parse_beethoven_vec","Beethoven.rmi") => parse_collect);
    }
}

/// Test the MIDI writer and parser.
///
/// Writing is only enabled with the `std` feature.
#[cfg(feature = "std")]
mod write {
    use super::*;

    #[test]
    fn clementi_rewrite() {
        test_rewrite!("rewrite_clementi", "Clementi.mid");
    }

    #[test]
    fn sandstorm_rewrite() {
        test_rewrite!("rewrite_sandstorm", "Sandstorm.mid");
    }

    #[test]
    #[cfg_attr(feature = "strict", should_panic)]
    fn pidamaged_rewrite() {
        test_rewrite!("rewrite_pidamaged", "PiDamaged.mid");
    }

    #[test]
    fn levels_rewrite() {
        test_rewrite!("rewrite_levels", "Levels.mid");
    }
}
