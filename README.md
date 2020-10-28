# Midly

Midly is a Standard Midi File parser and writer designed to be as efficient as
possible, making as few allocations as possible and using multiple threads to
parse and write MIDI tracks in parallel.

The behaviour of the parser is also configurable through crate features.
See the crate-level documentation for the available features and `no_std`
support.

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

Most types to be imported are on the crate root and are documented in-place.
Check the crate documentation for more information.
