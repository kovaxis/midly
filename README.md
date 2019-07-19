# Midly

## Getting started

First add the following line to your `Cargo.toml` file:

```
midly = "0.2"
```

Then use the `Smf` type in the root crate:

```
use std::fs;
use midly::Smf;

// Load bytes first
let data = fs::read("Pi.mid").unwrap();

// Parse the raw bytes
let smf = Smf::parse(&data).unwrap();

// Use the information
println!("midi file has {} tracks!", smf.tracks.len());
```

Most types to be imported are on the crate root and are documented in-place.
Check the crate documentation for more information.
