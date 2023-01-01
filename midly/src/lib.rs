mod prelude {
    pub use midly_core::{Bytes, Fps, MidiMessage};
    pub use std::{
        convert::{TryFrom, TryInto},
        error::Error as StdError,
        fmt,
        fs::File,
        io::{self, Read},
        path::Path,
        thread,
        time::{Duration, Instant},
    };
}

pub use midly_core;

#[cfg(feature = "midir-io")]
pub mod midir;

#[cfg(feature = "smf")]
pub use midly_smf as smf;

#[cfg(feature = "playback")]
pub mod playback;
