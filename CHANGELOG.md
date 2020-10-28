
# Version changelog

## 0.5

### 0.5.0

- Rename `number` module to `num`.
- Simplify generic `Smf<T>` to `Smf`, `SmfBytemap` and generic `parse`/`write` functions.
- Add the `alloc` feature, which can be disabled to make the crate fully `no_std` and make no
    allocations.
- Add the `parallel` feature to disable multithreading and the `rayon` dependency without dropping
    integration with `std`.
- Move error framework from `failure` to `std::error::Error` when `std` is enabled, and no error
    trait when disabled.
- Errors are always a thin pointer.
- Writing now supports `no_std` with an auxiliary `Write` trait.
- Event bytes no longer include delta-time.
- Optimized allocations by guessing the amount of bytes per event.
- Files without a correct header now fail early.
- Added a `PitchBend` newtype to manipulate pitch bend values.
- Added a `live` module that allows parsing standalone MIDI events.
- Added a `stream` module to support raw MIDI stream decoding.
- All types now implement `Debug`, and all data types implement `Hash`.
- `Smf::new` no longer returns an error, and creates an empty `Smf` with no tracks. To create an
    `Smf` with prebuilt tracks use `Smf { header, tracks }` construction.
- Added `Arena` to make track construction more ergonomic.

## 0.4

### 0.4.1

- Add support for the `.rmi` RIFF wrapper for MIDI files.
- Errors are now a thin pointer in release mode.
- Add a `TrackIter::running_status_mut` method.
- Update `README.md` to match `0.4`.

### 0.4.0

- `EventKind::parse` and `Event::read` no longer return event bytes.
- Simplify `lenient` and `strict` features to a simple `strict` feature.

## 0.3

## 0.3.0

- Add support for writing MIDI files and events.
- Handle running status more correctly.

## 0.2

### 0.2.2

- Fix pitch bend messages being read with the wrong endianness.

### 0.2.1

- Update `README.md` to match the API of `0.2`.
- Added an `ErrorKind::mesage` method.

### 0.2.0

- Move error framework from `error-chain` to `failure`.
- Renamed `Varlen` to `u28`.
- Added `lenient` and `strict` crate features to configure how eager should the parser be to reject
    files.
- Give meaningful names to MIDI message fields.
- Replace the `SmfBuffer` convenience type with three `parse` variants.
- Default the `Smf<T>` type to `Smf<Vec<Event>>`.
- No longer tries to parallelize lazy parsing.
- Renamed `EventKind::read` to `EventKind::parse` to match the rest of the parse methods.
- Added an optional (enabled by default) `std` feature to make the crate `no_std + alloc`.

## 0.1

### 0.1.3

- Rename `Fps::as_u8` and `Fps::from_u8` to `Fps::as_int` and `Fps::from_int`.

### 0.1.2

- Make MIDI primitives public.

### 0.1.1

- Add `as_int` method to convert MIDI integers to primitives.

### 0.1.0

- Initial release.

# Planned changes

- Unify names for MIDI/channel messages.
- Check thoroughly which parts of the API are exposed.
- Write docs.
- Expose a `std::io::Write`-based write API.
- Add `new` methods to all types to be more uniform.
- Try to optimize reading by using a cursor instead of a naive advancing slice.
- Revert stream to fire MIDI messages as soon as enough bytes arrive (this is pretty useful in
    real life).