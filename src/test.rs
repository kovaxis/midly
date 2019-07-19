use crate::Smf;
use failure::Fail;
use std::{time::Instant, fs};

/// Open and read the content of a file.
macro_rules! open {
    {$name:ident : $file:expr} => {
        let $name = fs::read(concat!("test-asset/", $file)).unwrap();
    };
}

/// Macro for parsing a MIDI file.
macro_rules! test {
    {($name:expr , $file:expr) => {$method_parse:ident,$method_len:ident}} => {{
        let counts = time($name, ||->Vec<_> {
            open!{smf: $file};
            let smf = match Smf::$method_parse(&smf) {
                Ok(smf) => smf,
                Err(err) => {
                    eprintln!("failed to parse test file:");
                    for cause in (&err as &Fail).iter_chain() {
                        eprintln!("  {}", cause);
                    }
                    panic!()
                },
            };
            smf.tracks.into_iter().map(|track| track.$method_len()).collect()
        });
        for (i,count) in counts.iter().enumerate() {
            println!("track {} has {} events", i, count);
        }
    }};
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
    fn clementi_defer() {
        test!(("parse_clementi_iter","Clementi.mid") => {parse_lazy,count});
    }
    #[test]
    fn clementi_collect() {
        test!(("parse_clementi_vec","Clementi.mid") => {parse,len});
    }

    #[test]
    fn pi_defer() {
        test!(("parse_pi_iter","Pi.mid") => {parse_lazy,count});
    }
    #[test]
    fn pi_collect() {
        test!(("parse_pi_vec","Pi.mid") => {parse,len});
    }
    
    #[test]
    #[cfg_attr(not(feature = "lenient"), should_panic)]
    fn pidamaged_defer() {
        test!(("parse_pidamaged_iter","PiDamaged.mid") => {parse_lazy,count});
    }
    #[test]
    #[cfg_attr(not(feature = "lenient"), should_panic)]
    fn pidamaged_collect() {
        test!(("parse_pidamaged_vec","PiDamaged.mid") => {parse,len});
    }

    #[test]
    fn levels_defer() {
        test!(("parse_levels_iter","LevelsAvicii.mid") => {parse_lazy,count});
    }
    #[test]
    #[cfg_attr(feature = "strict", should_panic)]
    fn levels_collect() {
        test!(("parse_levels_vec","LevelsAvicii.mid") => {parse,len});
    }
}
