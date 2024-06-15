use midly::Smf;
use std::{
    env,
    error::Error,
    fs,
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

const MIDI_DIR: &str = "../test-asset";

const MIDI_EXT: &[&str] = &["mid", "midi", "rmi"];

const INFO_COLLECTORS: &[(&str, fn(&Path) -> Result<(), Box<dyn Error>>)] = &[
    ("fs_vs_cpu", fs_vs_cpu),
    ("bytes_per_event", bytes_per_event),
];

fn fs_vs_cpu(path: &Path) -> Result<(), Box<dyn Error>> {
    let deadline = Instant::now() + Duration::from_millis(1000);
    let mut first = None;
    let mut total_fs = Duration::from_secs(0);
    let mut total_cpu = Duration::from_secs(0);
    let mut iters = 0;
    loop {
        let a = Instant::now();
        let data = fs::read(path)?;
        let b = Instant::now();
        let smf = Smf::parse(&data)?;
        let c = Instant::now();
        if let Some((_, _, track_count)) = first {
            total_fs += b - a;
            total_cpu += c - b;
            assert_eq!(track_count, smf.tracks.len());
            iters += 1;
        } else {
            first = Some((b - a, c - b, smf.tracks.len()));
        }
        if c >= deadline {
            break;
        }
    }
    let fmt_duration = |duration: Duration| (duration.as_micros() / 100) as f32 / 10.0;
    if iters > 0 {
        eprintln!(
            "fs: {}ms / cpu: {}ms in {} hot iters",
            fmt_duration(total_fs / iters),
            fmt_duration(total_cpu / iters),
            iters
        )
    } else if let Some((fs_time, cpu_time, _)) = first {
        eprintln!(
            "fs: {}ms / cpu: {}ms in 1 cold iter",
            fmt_duration(fs_time),
            fmt_duration(cpu_time),
        )
    } else {
        unreachable!();
    }
    Ok(())
}

fn bytes_per_event(path: &Path) -> Result<(), Box<dyn Error>> {
    let file = fs::read(path)?;
    let (_header, tracks) = midly::parse(&file)?;
    let mut total_bytes = 0;
    let mut total_events = 0;
    let mut min_bpe = std::f64::INFINITY;
    let mut max_bpe = 0.0;
    let bpe = tracks
        .map(|track| {
            let track = track?;
            let track_bytes = track.unread().len();
            let track_events = track.count();
            total_bytes += track_bytes;
            total_events += track_events;
            let bpe = track_bytes as f64 / track_events as f64;
            if bpe < min_bpe {
                min_bpe = bpe;
            }
            if bpe > max_bpe {
                max_bpe = bpe;
            }
            Ok((track_bytes, track_events))
        })
        .collect::<Result<Vec<_>, Box<dyn Error>>>()?;
    eprintln!(
        "bytes/event: min {} / avg {} / max {}",
        min_bpe,
        total_bytes as f64 / total_events as f64,
        max_bpe,
    );
    for (idx, &(bytes, events)) in bpe.iter().enumerate() {
        eprintln!(
            "        track {}: {} bytes/event ({} bytes in {} events)",
            idx,
            bytes as f64 / events as f64,
            bytes,
            events,
        );
    }
    Ok(())
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

fn main() {
    let midi_filter = env::args().nth(1).unwrap_or_default().to_lowercase();
    let info_filter = env::args().nth(2).unwrap_or_default().to_lowercase();
    let midi_dir = env::args().nth(3).unwrap_or(MIDI_DIR.to_string());

    let collectors = INFO_COLLECTORS
        .iter()
        .filter(|(name, _)| name.contains(&info_filter))
        .collect::<Vec<_>>();
    if collectors.is_empty() {
        eprintln!("no info collectors match the pattern \"{}\"", info_filter);
        eprint!("available info collectors: ");
        for (i, (name, _)) in INFO_COLLECTORS.iter().enumerate() {
            if i > 0 {
                eprint!(", ");
            }
            eprint!("{}", name);
        }
        eprintln!();
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
            //Parse this file
            eprintln!("collecting info about file \"{}\"", midi.display());
            for &(name, collect) in collectors.iter() {
                eprint!("  {}: ", name);
                match collect(&midi) {
                    Ok(()) => {}
                    Err(err) => {
                        eprintln!("collector error ({})", err);
                    }
                }
            }
            eprintln!();
        }
    }
}
