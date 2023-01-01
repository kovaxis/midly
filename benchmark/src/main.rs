use std::{
    env, fs,
    path::{Path, PathBuf},
    time::Instant,
};

const MIDI_DIR: &str = "../test-asset";

const MIDI_EXT: &[&str] = &["mid", "midi", "rmi"];

const PARSERS: &[(&str, fn(&Path) -> Result<usize, String>)] = &[
    (&"midly", parse_midly),
    (&"nom-midi", parse_nom),
    (&"rimd", parse_rimd),
    (&"augmented-midi", parse_augmented_midi),
];

fn parse_midly(path: &Path) -> Result<usize, String> {
    let data = fs::read(path).map_err(|err| format!("{}", err))?;
    let smf = midly::Smf::parse(&data).map_err(|err| format!("{}", err))?;
    Ok(smf.tracks.len())
}

fn parse_nom(path: &Path) -> Result<usize, String> {
    let data = fs::read(path).map_err(|err| format!("{}", err))?;
    let smf = nom_midi::parser::parse_smf(&data)
        .map_err(|err| format!("{}", err))?
        .1;
    Ok(smf.tracks.len())
}

fn parse_rimd(path: &Path) -> Result<usize, String> {
    let smf = rimd::SMF::from_file(path).map_err(|err| format!("{}", err))?;
    Ok(smf.tracks.len())
}

fn parse_augmented_midi(path: &Path) -> Result<usize, String> {
    let data = fs::read(path).map_err(|err| format!("{}", err))?;
    let smf: augmented_midi::MIDIFile<&str, &[u8]> = augmented_midi::parse_midi_file(&data)
        .map_err(|err| format!("{}", err))?
        .1;
    Ok(smf.chunks.len())
}

fn list_midis(dir: &Path) -> Vec<PathBuf> {
    let mut midis = Vec::new();
    for entry in fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if MIDI_EXT
            .iter()
            .any(|ext| path.extension() == Some(ext.as_ref()))
        {
            midis.push(path);
        }
    }
    midis
}

fn use_parser(parse: fn(&Path) -> Result<usize, String>, path: &Path) -> Result<(), String> {
    let round = |num: f64| (num * 100.0).round() / 100.0;

    let runtime = || -> Result<_, String> {
        let start = Instant::now();
        let out = parse(path)?;
        let time = round((start.elapsed().as_micros() as f64) / 1000.0);
        Ok((out, time))
    };

    let (track_count, cold_time) = runtime()?;
    let runtime = || -> Result<_, String> {
        let (out, time) = runtime()?;
        assert_eq!(
            out, track_count,
            "parser is not consistent with track counts"
        );
        Ok(time)
    };

    let iters = (2000.0 / cold_time).floor() as u64 + 1;
    let mut total_time = 0.0;
    let mut max_time = cold_time;
    let mut min_time = cold_time;
    for _ in 0..iters {
        let time = runtime()?;
        total_time += time;
        max_time = max_time.max(time);
        min_time = min_time.min(time);
    }
    let avg_time = round(total_time / (iters as f64));

    eprintln!(
        "{} tracks in {} iters / min {} / avg {} / max {}",
        track_count, iters, min_time, avg_time, max_time
    );

    Ok(())
}

fn main() {
    let midi_filter = env::args().nth(1).unwrap_or_default().to_lowercase();
    let parser_filter = env::args().nth(2).unwrap_or_default().to_lowercase();
    let midi_dir = env::args().nth(3).unwrap_or(MIDI_DIR.to_string());

    let parsers = PARSERS
        .iter()
        .filter(|(name, _)| name.contains(&parser_filter))
        .collect::<Vec<_>>();
    if parsers.is_empty() {
        eprintln!("no parsers match the pattern \"{}\"", parser_filter);
        eprint!("available parsers: ");
        for (i, (name, _)) in PARSERS.iter().enumerate() {
            if i > 0 {
                eprint!(", ");
            }
            eprint!("{}", name);
        }
    }

    let unfiltered_midis = list_midis(midi_dir.as_ref());
    let midis = unfiltered_midis
        .iter()
        .filter(|midi| {
            midi.file_name()
                .unwrap_or_default()
                .to_str()
                .expect("non-utf8 file")
                .to_lowercase()
                .contains(&midi_filter)
        })
        .collect::<Vec<_>>();
    if midis.is_empty() {
        eprintln!("no midi files match the pattern \"{}\"", midi_filter);
        eprintln!("available midi files:");
        for file in unfiltered_midis.iter() {
            eprintln!("  {}", file.display());
        }
    } else {
        for midi in midis {
            //Read file to warm up file cache
            let size = std::fs::read(midi).map(|b| b.len()).unwrap_or(0);
            //Parse this file
            eprintln!("parsing file \"{}\" ({} KB)", midi.display(), size / 1024);
            for &(name, parse) in parsers.iter() {
                eprint!("  {}: ", name);
                match use_parser(*parse, &midi) {
                    Ok(()) => {}
                    Err(_err) => {
                        eprintln!("parse error");
                    }
                }
            }
            eprintln!();
        }
    }
}
