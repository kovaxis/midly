# Midly

Midly is a MIDI decoder and encoder designed for efficiency and completeness, supporting both
`.mid` files and real-time MIDI streams while making minimal allocations.

See the crate-level documentation for the available features and `no_std` support.

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
`midly` library, providing automatic multithreading and minimal allocations.
The following chart presents benchmark results against other MIDI libraries in the ecosystem capable
of reading `.mid` files. The benchmarks were done on a warm file cache.

| File name       | File size | `rimd 0.0.1` | `nom-midi 0.5.1` | `midly 0.5.2` |
| --------------- | --------- | ------------ | ---------------- | ------------- |
| `Clementi.mid`  | 4 KB      | 11 ms        | Error            | 0.15 ms       |
| `CrabRave.mid`  | 53 KB     | 145 ms       | 0.55 ms          | 0.26 ms       |
| `Beethoven.rmi` | 90 KB     | Error        | Error            | 0.48 ms       |
| `Pi.mid`        | 24 MB     | 66700 ms     | 358 ms           | 85 ms         |
| `PiDamaged.mid` | 64 KB     | Freeze       | Error            | 0.55 ms       |

The above results are only referential, actual performance depends on the hardware and operating
system.
