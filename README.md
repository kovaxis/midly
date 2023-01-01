# Midly

Midly is a feature-complete MIDI decoder and encoder designed for efficiency and ease of use.

See the [crate-level documentation](https://docs.rs/midly) for examples, detailed documentation and
available cargo features.

## Features

- Supports both `.mid` files and real-time MIDI packets.
- Very complete, supports reading and writing while handling all edge cases in the MIDI spec.
- Simple API, just load your data and call `Smf::parse` or `LiveEvent::parse`.
- Optional `no_std` and no `alloc` support.
- Zero-copy, all MIDI types simply reference the original buffer.
- Fast! See the [speed](#speed) section below.

## Getting started

First add the following line to your `Cargo.toml` file, under the
`[dependencies]` section:

```toml
midly = "0.5"
```

Then use the `Smf` type in the crate root:

```rust
// Load bytes first
let data = std::fs::read("Pi.mid").unwrap();

// Parse the raw bytes
let mut smf = midly::Smf::parse(&data).unwrap();

// Use the information
println!("midi file has {} tracks!", smf.tracks.len());

// Modify the file
smf.header.format = midly::Format::Sequential;

// Save it back
smf.save("PiRewritten.mid").unwrap();
```

Or use the `LiveEvent` type to parse real-time MIDI events:

```rust
use midly::{live::LiveEvent, MidiMessage};

fn on_midi(event: &[u8]) {
    let event = LiveEvent::parse(event).unwrap();
    match event {
        LiveEvent::Midi { channel, message } => match message {
            MidiMessage::NoteOn { key, vel } => {
                println!("hit note {} on channel {}", key, channel);
            }
            _ => {}
        },
        _ => {}
    }
}
```

Most types to be imported are on the crate root and are documented in-place.
Check the [crate documentation](https://docs.rs/midly) for more information.

## Speed

Although performance is not critical in a MIDI library, it still is an important objective of the
`midly` library, providing automatic multithreading for large files and minimal allocations.
The following chart presents benchmark results against other MIDI libraries in the ecosystem capable
of reading `.mid` files.

| File name       | File size | `rimd 0.0.1` | `nom-midi 0.5.1` | `augmented-midi 1.3.0` | `midly 0.5.3` |
| --------------- | --------- | ------------ | ---------------- | ---------------------- | ------------- |
| `Clementi.mid`  | 4 KB      | 4 ms         | Error            | **0.06 ms**            | 0.07 ms       |
| `CrabRave.mid`  | 53 KB     | 4 ms         | 0.48 ms          | 0.53 ms                | **0.15 ms**   |
| `Beethoven.rmi` | 90 KB     | Error        | Error            | Error                  | **0.48 ms**   |
| `Pi.mid`        | 24 MB     | 20575 ms     | 253 ms           | 214 ms                 | **60 ms**     |
| `PiDamaged.mid` | 64 KB     | Freeze       | Error            | Error                  | **0.41 ms**   |

The above results are only referential, actual performance depends wildly on the hardware and operating
system.
The benchmarks were done on a Linux x64 machine with a warm file cache.
The benchmark code is available in the `/benchmark` directory in the source.
