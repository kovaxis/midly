//! All sort of events and their parsers.

use crate::{
    live::{LiveEvent, SystemCommon},
    prelude::*,
    primitive::{read_varlen_slice, write_varlen_slice, SmpteTime},
};

/// Represents a parsed SMF track event.
///
/// Consists of a delta time (in MIDI ticks relative to the previous event) and the actual track
/// event.
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct TrackEvent<'a> {
    /// How many MIDI ticks after the previous event should this event fire.
    pub delta: u28,
    /// The type of event along with event-specific data.
    pub kind: TrackEventKind<'a>,
}
impl<'a> TrackEvent<'a> {
    /// Advances the slice and updates `running_status`.
    ///
    /// In case of failure the slice might be left in the middle of an event!
    pub(crate) fn read(
        raw: &mut &'a [u8],
        running_status: &mut Option<u8>,
    ) -> Result<TrackEvent<'a>> {
        let delta = u28::read_u7(raw).context(err_invalid!("failed to read event deltatime"))?;
        let kind = TrackEventKind::read(raw, running_status)
            .context(err_invalid!("failed to parse event"))?;
        Ok(TrackEvent { delta, kind })
    }

    pub(crate) fn read_bytemap(
        raw: &mut &'a [u8],
        running_status: &mut Option<u8>,
    ) -> Result<(&'a [u8], TrackEvent<'a>)> {
        let delta = u28::read_u7(raw).context(err_invalid!("failed to read event deltatime"))?;
        let old_raw = *raw;
        let kind = TrackEventKind::read(raw, running_status)
            .context(err_invalid!("failed to parse event"))?;
        Ok((
            &old_raw[..old_raw.len() - raw.len()],
            TrackEvent { delta, kind },
        ))
    }

    pub(crate) fn write<W: Write>(
        &self,
        running_status: &mut Option<u8>,
        out: &mut W,
    ) -> WriteResult<W> {
        self.delta.write_varlen(out)?;
        self.kind.write(running_status, out)?;
        Ok(())
    }

    /// Remove any lifetimed data from this event to create a `TrackEvent` with `'static`
    /// lifetime that can be stored and moved everywhere, solving borrow checker issues.
    ///
    /// WARNING: Any bytestrings in the input will be replaced by empty bytestrings.
    pub fn to_static(&self) -> TrackEvent<'static> {
        TrackEvent {
            delta: self.delta,
            kind: self.kind.to_static(),
        }
    }
}

/// Represents the different kinds of SMF events and their associated data.
///
/// It notably does *not* include the timing of the event; the `TrackEvent` struct is responsible
/// for this.
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum TrackEventKind<'a> {
    /// A message associated to a MIDI channel carrying musical data.
    ///
    /// Usually, the bulk of MIDI data is these kind of messages.
    Midi {
        /// The MIDI channel that this event is associated with.
        channel: u4,
        /// The MIDI message type and associated data.
        message: MidiMessage,
    },
    /// A System Exclusive message, carrying arbitrary data.
    ///
    /// The data bytes included here do not include the implicit `0xF0` prefix.
    ///
    /// Usually SysEx events end with an `0xF7` byte, but SysEx events that are split into several
    /// small packets may only contain the `0xF7` byte in the last packet fragment.
    SysEx(&'a [u8]),
    /// An escape sequence, intended to send arbitrary data to the MIDI synthesizer.
    Escape(&'a [u8]),
    /// A meta-message, giving extra information for correct playback, like tempo, song name,
    /// lyrics, etc...
    Meta(MetaMessage<'a>),
}
impl<'a> TrackEventKind<'a> {
    fn read(raw: &mut &'a [u8], running_status: &mut Option<u8>) -> Result<TrackEventKind<'a>> {
        //Read status
        let mut status = *raw.get(0).ok_or(err_invalid!("failed to read status"))?;
        if status < 0x80 {
            //Running status!
            status = running_status.ok_or(err_invalid!(
                "event missing status with no running status active"
            ))?;
        } else {
            //Advance slice 1 byte to consume status. Note that because we already did `get()`, we
            //can use panicking index here
            *raw = &raw[1..];
        }
        //Delegate further parsing depending on status
        let kind = match status {
            0x80..=0xEF => {
                *running_status = Some(status);
                let data = MidiMessage::read_data_u8(status, raw)?;
                let (channel, message) = MidiMessage::read(status, data);
                TrackEventKind::Midi { channel, message }
            }
            0xFF => {
                *running_status = None;
                TrackEventKind::Meta(
                    MetaMessage::read(raw).context(err_invalid!("failed to read meta event"))?,
                )
            }
            0xF0 => {
                *running_status = None;
                TrackEventKind::SysEx(
                    read_varlen_slice(raw).context(err_invalid!("failed to read sysex event"))?,
                )
            }
            0xF7 => {
                *running_status = None;
                TrackEventKind::Escape(
                    read_varlen_slice(raw).context(err_invalid!("failed to read escape event"))?,
                )
            }
            0xF1..=0xF6 => bail!(err_invalid!(
                "standard midi files cannot contain system common events"
            )),
            0xF8..=0xFE => bail!(err_invalid!(
                "standard midi files cannot contain system realtime events"
            )),
            0x00..=0x7F => panic!("invalid running status without top bit set"),
        };
        Ok(kind)
    }

    /// Writes a single event to the given output writer.
    ///
    /// `running_status` keeps track of the last MIDI status, in order to make proper use of
    /// running status. It should be shared between consecutive calls, and should initially be set
    /// to `None`.
    fn write<W: Write>(&self, running_status: &mut Option<u8>, out: &mut W) -> WriteResult<W> {
        //Running Status rules:
        // - MIDI Messages (0x80 ..= 0xEF) alter and use running status
        // - System Exclusive (0xF0) cancels and cannot use running status
        // - Escape (0xF7) cancels and cannot use running status
        // - Meta Messages (0xFF) cancel and cannot use running status
        match self {
            TrackEventKind::Midi { channel, message } => {
                let status = message.status_nibble() << 4 | channel.as_int();
                if Some(status) != *running_status {
                    //Explicitly write status
                    out.write(&[status])?;
                    *running_status = Some(status);
                }
                message.write(out)?;
            }
            TrackEventKind::SysEx(data) => {
                *running_status = None;
                out.write(&[0xF0])?;
                write_varlen_slice(data, out)?;
            }
            TrackEventKind::Escape(data) => {
                *running_status = None;
                out.write(&[0xF7])?;
                write_varlen_slice(data, out)?;
            }
            TrackEventKind::Meta(meta) => {
                *running_status = None;
                out.write(&[0xFF])?;
                meta.write(out)?;
            }
        }
        Ok(())
    }

    /// Lossy conversion from a track event to a live event.
    ///
    /// Only channel MIDI messages and not-split SysEx messages can be converted.
    /// Meta messages and arbitrary escapes yield `None` when converted.
    pub fn as_live_event(&self) -> Option<LiveEvent<'a>> {
        match self {
            TrackEventKind::Midi { channel, message } => Some(LiveEvent::Midi {
                channel: *channel,
                message: *message,
            }),
            TrackEventKind::SysEx(data) => {
                if data.last() == Some(&0xF7) {
                    let data_u7 = u7::slice_from_int(data);
                    if data_u7.len() == data.len() - 1 {
                        return Some(LiveEvent::Common(SystemCommon::SysEx(data_u7)));
                    }
                }
                None
            }
            TrackEventKind::Escape(_data) => None,
            TrackEventKind::Meta(_meta) => None,
        }
    }

    /// Remove any lifetimed data from this event to create a `TrackEventKind` with `'static`
    /// lifetime that can be stored and moved everywhere, solving borrow checker issues.
    ///
    /// WARNING: Any bytestrings in the input will be replaced by empty bytestrings.
    pub fn to_static(&self) -> TrackEventKind<'static> {
        use self::TrackEventKind::*;
        match *self {
            Midi { channel, message } => Midi { channel, message },
            SysEx(_) => SysEx(b""),
            Escape(_) => Escape(b""),
            Meta(meta) => Meta(meta.to_static()),
        }
    }
}

/// Represents a MIDI message, usually associated to a MIDI channel.
///
/// If you wish to parse a MIDI message from a slice of raw MIDI bytes, use the
/// [`LiveEvent::parse`](live/enum.LiveEvent.html#method.parse) method instead and ignore all
/// variants except for [`LiveEvent::Midi`](live/enum.LiveEvent.html#variant.Midi).
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum MidiMessage {
    /// Stop playing a note.
    NoteOff {
        /// The MIDI key to stop playing.
        key: u7,
        /// The velocity with which to stop playing it.
        vel: u7,
    },
    /// Start playing a note.
    NoteOn {
        /// The key to start playing.
        key: u7,
        /// The velocity (strength) with which to press it.
        ///
        /// Note that by convention a `NoteOn` message with a velocity of 0 is equivalent to a
        /// `NoteOff`.
        vel: u7,
    },
    /// Modify the velocity of a note after it has been played.
    Aftertouch {
        /// The key for which to modify its velocity.
        key: u7,
        /// The new velocity for the key.
        vel: u7,
    },
    /// Modify the value of a MIDI controller.
    Controller {
        /// The controller to modify.
        ///
        /// See the MIDI spec for the meaning of each index.
        controller: u7,
        /// The value to set it to.
        value: u7,
    },
    /// Change the program (also known as instrument) for a channel.
    ProgramChange {
        /// The new program (instrument) to use for the channel.
        program: u7,
    },
    /// Change the note velocity of a whole channel at once, without starting new notes.
    ChannelAftertouch {
        /// The new velocity for all notes currently playing in the channel.
        vel: u7,
    },
    /// Set the pitch bend value for the entire channel.
    PitchBend {
        /// The new pitch-bend value.
        bend: PitchBend,
    },
}
impl MidiMessage {
    /// Midi messages have a known length.
    pub(crate) fn msg_length(status: u8) -> usize {
        const LENGTH_BY_STATUS: [u8; 16] = [0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 1, 1, 2, 0];
        LENGTH_BY_STATUS[(status >> 4) as usize] as usize
    }

    /// Extract the data bytes from a raw slice.
    pub(crate) fn read_data_u8(status: u8, raw: &mut &[u8]) -> Result<[u7; 2]> {
        let len = Self::msg_length(status);
        let data = raw
            .split_checked(len)
            .ok_or_else(|| err_invalid!("truncated midi message"))?;
        Ok(match len {
            1 => [u7::check_int(data[0])?, u7::from(0)],
            2 => [u7::check_int(data[0])?, u7::check_int(data[1])?],
            _ => [u7::from(0), u7::from(0)],
        })
    }

    /// Get the data bytes from a databyte slice.
    pub(crate) fn get_data_u7(status: u8, data: &[u7]) -> Result<[u7; 2]> {
        let len = Self::msg_length(status);
        ensure!(data.len() >= len, err_invalid!("truncated midi message"));
        Ok(match len {
            1 => [data[0], u7::from(0)],
            2 => [data[0], data[1]],
            _ => [u7::from(0), u7::from(0)],
        })
    }

    /// Receives status byte and midi args separately.
    ///
    /// Panics if the `status` is not a MIDI message status (0x80..=0xEF).
    pub(crate) fn read(status: u8, data: [u7; 2]) -> (u4, MidiMessage) {
        let channel = u4::from(status);
        let msg = match status >> 4 {
            0x8 => MidiMessage::NoteOff {
                key: data[0],
                vel: data[1],
            },
            0x9 => MidiMessage::NoteOn {
                key: data[0],
                vel: data[1],
            },
            0xA => MidiMessage::Aftertouch {
                key: data[0],
                vel: data[1],
            },
            0xB => MidiMessage::Controller {
                controller: data[0],
                value: data[1],
            },
            0xC => MidiMessage::ProgramChange { program: data[0] },
            0xD => MidiMessage::ChannelAftertouch { vel: data[0] },
            0xE => {
                //Note the little-endian order, contrasting with the default big-endian order of
                //Standard Midi Files
                let lsb = data[0].as_int() as u16;
                let msb = data[1].as_int() as u16;
                MidiMessage::PitchBend {
                    bend: PitchBend(u14::from(msb << 7 | lsb)),
                }
            }
            _ => panic!("parsed midi message before checking that status is in range"),
        };
        (channel, msg)
    }
    /// Get the raw status nibble for this MIDI message type.
    pub(crate) fn status_nibble(&self) -> u8 {
        match self {
            MidiMessage::NoteOff { .. } => 0x8,
            MidiMessage::NoteOn { .. } => 0x9,
            MidiMessage::Aftertouch { .. } => 0xA,
            MidiMessage::Controller { .. } => 0xB,
            MidiMessage::ProgramChange { .. } => 0xC,
            MidiMessage::ChannelAftertouch { .. } => 0xD,
            MidiMessage::PitchBend { .. } => 0xE,
        }
    }
    /// Write the data part of this message, not including the status.
    pub(crate) fn write<W: Write>(&self, out: &mut W) -> WriteResult<W> {
        match self {
            MidiMessage::NoteOff { key, vel } => out.write(&[key.as_int(), vel.as_int()])?,
            MidiMessage::NoteOn { key, vel } => out.write(&[key.as_int(), vel.as_int()])?,
            MidiMessage::Aftertouch { key, vel } => out.write(&[key.as_int(), vel.as_int()])?,
            MidiMessage::Controller { controller, value } => {
                out.write(&[controller.as_int(), value.as_int()])?
            }
            MidiMessage::ProgramChange { program } => out.write(&[program.as_int()])?,
            MidiMessage::ChannelAftertouch { vel } => out.write(&[vel.as_int()])?,
            MidiMessage::PitchBend { bend } => {
                let raw = bend.0.as_int();
                out.write(&[(raw & 0x7F) as u8, (raw >> 7) as u8])?
            }
        }
        Ok(())
    }
}

/// The value of a pitch bend, represented as 14 bits.
///
/// A value of `0x0000` indicates full bend downwards.
/// A value of `0x2000` indicates no bend.
/// A value of `0x3FFF` indicates full bend upwards.
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct PitchBend(pub u14);
impl PitchBend {
    /// The minimum value of `0x0000`, indicating full bend downwards.
    #[inline]
    pub const fn min_raw_value() -> PitchBend {
        PitchBend(u14::new(0x0000))
    }

    /// The middle value of `0x2000`, indicating no bend.
    #[inline]
    pub const fn mid_raw_value() -> PitchBend {
        PitchBend(u14::new(0x2000))
    }

    /// The maximum value of `0x3FFF`, indicating full bend upwards.
    #[inline]
    pub const fn max_raw_value() -> PitchBend {
        PitchBend(u14::new(0x3FFF))
    }

    /// Create a `PitchBend` value from an int in the range `[-0x2000, 0x1FFF]`.
    ///
    /// Integers outside this range will be clamped.
    #[inline]
    pub fn from_int(int: i16) -> PitchBend {
        PitchBend(u14::new((int.max(-0x2000).min(0x1FFF) + 0x2000) as u16))
    }

    /// Create a `PitchBend` value from a number in the range `[-1.0, 1.0)`.
    ///
    /// Floats outside this range will be clamped.
    #[inline]
    pub fn from_f32(float: f32) -> PitchBend {
        PitchBend::from_int((float.max(-1.0).min(1.0) * 0x2000 as f32) as i16)
    }

    /// Create a `PitchBend` value from a number in the range `[-1.0, 1.0)`.
    ///
    /// Floats outside this range will be clamped.
    #[inline]
    pub fn from_f64(float: f64) -> PitchBend {
        PitchBend::from_int((float.max(-1.0).min(1.0) * 0x2000 as f64) as i16)
    }

    /// Returns an int in the range `[-0x2000, 0x1FFF]`.
    #[inline]
    pub fn as_int(self) -> i16 {
        self.0.as_int() as i16 - 0x2000
    }

    /// Returns an `f32` in the range `[-1.0, 1.0)`.
    #[inline]
    pub fn as_f32(self) -> f32 {
        self.as_int() as f32 * (1.0 / 0x2000 as f32)
    }

    /// Returns an `f64` in the range `[-1.0, 1.0)`.
    #[inline]
    pub fn as_f64(self) -> f64 {
        self.as_int() as f64 * (1.0 / 0x2000 as f64)
    }
}

/// A "meta message", as defined by the SMF spec.
/// These events carry metadata about the track, such as tempo, time signature, copyright, etc...
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum MetaMessage<'a> {
    /// For `Format::Sequential` MIDI file types, `TrackNumber` can be empty, and defaults to
    /// the track index.
    TrackNumber(Option<u16>),
    /// Arbitrary text associated to an instant.
    Text(&'a [u8]),
    /// A copyright notice.
    Copyright(&'a [u8]),
    /// Information about the name of the track.
    TrackName(&'a [u8]),
    /// Information about the name of the current instrument.
    InstrumentName(&'a [u8]),
    /// Arbitrary lyric information associated to an instant.
    Lyric(&'a [u8]),
    /// Arbitrary marker text associated to an instant.
    Marker(&'a [u8]),
    /// Arbitrary cue point text associated to an instant.
    CuePoint(&'a [u8]),
    /// Information about the name of the current program.
    ProgramName(&'a [u8]),
    /// Name of the device that this file was intended to be played with.
    DeviceName(&'a [u8]),
    /// Number of the MIDI channel that this file was intended to be played with.
    MidiChannel(u4),
    /// Number of the MIDI port that this file was intended to be played with.
    MidiPort(u7),
    /// Obligatory at track end.
    EndOfTrack,
    /// Amount of microseconds per beat (quarter note).
    ///
    /// Usually appears at the beginning of a track, before any midi events are sent, but there
    /// are no guarantees.
    Tempo(u24),
    /// The MIDI SMPTE offset meta message specifies an offset for the starting point of a MIDI
    /// track from the start of a sequence in terms of SMPTE time (hours:minutes:seconds:frames:subframes).
    ///
    /// [Reference](https://www.recordingblogs.com/wiki/midi-smpte-offset-meta-message)
    SmpteOffset(SmpteTime),
    /// In order of the MIDI specification, numerator, denominator, MIDI clocks per click, 32nd
    /// notes per quarter
    TimeSignature(u8, u8, u8, u8),
    /// As in the MIDI specification, negative numbers indicate number of flats and positive
    /// numbers indicate number of sharps.
    /// `false` indicates a major scale, `true` indicates a minor scale.
    KeySignature(i8, bool),
    /// Arbitrary data intended for the sequencer.
    /// This data is never sent to a device.
    SequencerSpecific(&'a [u8]),
    /// An unknown or malformed meta-message.
    ///
    /// The first `u8` is the raw meta-message identifier byte.
    /// The slice is the actual payload of the meta-message.
    Unknown(u8, &'a [u8]),
}
impl<'a> MetaMessage<'a> {
    /// Remove any lifetimed data from this event to create a `MidiMessage` with `'static` lifetime
    /// that can be stored and moved everywhere, solving borrow checker issues.
    ///
    /// WARNING: Any bytestrings in the input will be replaced by empty bytestrings.
    pub fn to_static(&self) -> MetaMessage<'static> {
        use self::MetaMessage::*;
        match *self {
            TrackNumber(v) => TrackNumber(v),
            Text(_) => Text(b""),
            Copyright(_) => Copyright(b""),
            TrackName(_) => TrackName(b""),
            InstrumentName(_) => InstrumentName(b""),
            Lyric(_) => Lyric(b""),
            Marker(_) => Marker(b""),
            CuePoint(_) => CuePoint(b""),
            ProgramName(_) => ProgramName(b""),
            DeviceName(_) => DeviceName(b""),
            MidiChannel(v) => MidiChannel(v),
            MidiPort(v) => MidiPort(v),
            EndOfTrack => EndOfTrack,
            Tempo(v) => Tempo(v),
            SmpteOffset(v) => SmpteOffset(v),
            TimeSignature(v0, v1, v2, v3) => TimeSignature(v0, v1, v2, v3),
            KeySignature(v0, v1) => KeySignature(v0, v1),
            SequencerSpecific(_) => SequencerSpecific(b""),
            Unknown(v, _) => Unknown(v, b""),
        }
    }

    #[allow(clippy::len_zero)]
    fn read(raw: &mut &'a [u8]) -> Result<MetaMessage<'a>> {
        let type_byte = u8::read(raw).context(err_invalid!("failed to read meta message type"))?;
        let mut data =
            read_varlen_slice(raw).context(err_invalid!("failed to read meta message data"))?;
        Ok(match type_byte {
            0x00 => MetaMessage::TrackNumber({
                if data.len() >= 2 {
                    Some(u16::read(&mut data)?)
                } else {
                    None
                }
            }),
            0x01 => MetaMessage::Text(data),
            0x02 => MetaMessage::Copyright(data),
            0x03 => MetaMessage::TrackName(data),
            0x04 => MetaMessage::InstrumentName(data),
            0x05 => MetaMessage::Lyric(data),
            0x06 => MetaMessage::Marker(data),
            0x07 => MetaMessage::CuePoint(data),
            0x08 => MetaMessage::ProgramName(data),
            0x09 => MetaMessage::DeviceName(data),
            0x20 if data.len() >= 1 => MetaMessage::MidiChannel(u4::read(&mut data)?),
            0x21 if data.len() >= 1 => MetaMessage::MidiPort(u7::read(&mut data)?),
            0x2F => MetaMessage::EndOfTrack,
            0x51 if data.len() >= 3 => MetaMessage::Tempo(u24::read(&mut data)?),
            0x54 if data.len() >= 5 => MetaMessage::SmpteOffset(
                SmpteTime::read(&mut data).context(err_invalid!("failed to read smpte time"))?,
            ),
            0x58 if data.len() >= 4 => MetaMessage::TimeSignature(
                u8::read(&mut data)?,
                u8::read(&mut data)?,
                u8::read(&mut data)?,
                u8::read(&mut data)?,
            ),
            0x59 => {
                MetaMessage::KeySignature(u8::read(&mut data)? as i8, u8::read(&mut data)? != 0)
            }
            0x7F => MetaMessage::SequencerSpecific(data),
            _ => MetaMessage::Unknown(type_byte, data),
        })
    }
    fn write<W: Write>(&self, out: &mut W) -> WriteResult<W> {
        let mut write_msg = |type_byte: u8, data: &[u8]| {
            out.write(&[type_byte])?;
            write_varlen_slice(data, out)?;
            Ok(())
        };
        match self {
            MetaMessage::TrackNumber(track_num) => match track_num {
                None => write_msg(0x00, &[]),
                Some(track_num) => write_msg(0x00, &track_num.to_be_bytes()[..]),
            },
            MetaMessage::Text(data) => write_msg(0x01, data),
            MetaMessage::Copyright(data) => write_msg(0x02, data),
            MetaMessage::TrackName(data) => write_msg(0x03, data),
            MetaMessage::InstrumentName(data) => write_msg(0x04, data),
            MetaMessage::Lyric(data) => write_msg(0x05, data),
            MetaMessage::Marker(data) => write_msg(0x06, data),
            MetaMessage::CuePoint(data) => write_msg(0x07, data),
            MetaMessage::ProgramName(data) => write_msg(0x08, data),
            MetaMessage::DeviceName(data) => write_msg(0x09, data),
            MetaMessage::MidiChannel(chan) => write_msg(0x20, &[chan.as_int()]),
            MetaMessage::MidiPort(port) => write_msg(0x21, &[port.as_int()]),
            MetaMessage::EndOfTrack => write_msg(0x2F, &[]),
            MetaMessage::Tempo(microsperbeat) => {
                write_msg(0x51, &microsperbeat.as_int().to_be_bytes()[1..])
            }
            MetaMessage::SmpteOffset(smpte) => write_msg(0x54, &smpte.encode()[..]),
            MetaMessage::TimeSignature(num, den, ticksperclick, thirtysecondsperquarter) => {
                write_msg(
                    0x58,
                    &[*num, *den, *ticksperclick, *thirtysecondsperquarter],
                )
            }
            MetaMessage::KeySignature(sharps, minor) => {
                write_msg(0x59, &[*sharps as u8, *minor as u8])
            }
            MetaMessage::SequencerSpecific(data) => write_msg(0x7F, data),
            MetaMessage::Unknown(type_byte, data) => write_msg(*type_byte, data),
        }
    }
}
