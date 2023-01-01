//! All sort of events and their parsers.

use crate::prelude::*;
use midly_core::{ChannelMessage, PitchBend, SmpteTime};

/// Represents a single event in a track within an SMF file.
///
/// Consists of a delta time (in MIDI ticks relative to the previous event) and the actual MIDI
/// message.
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct TrackEvent<'a> {
    /// How many MIDI ticks after the previous event should this event fire.
    pub delta: u32,
    /// The MIDI message that should be fired at this time.
    pub msg: MidiMessage<'a>,
}
impl<'a> TrackEvent<'a> {
    /// Advances the slice and updates `running_status`.
    ///
    /// In case of failure the slice might be left in the middle of an event!
    #[inline]
    pub(crate) fn read(
        raw: &mut &'a mut [u8],
        running_status: &mut Option<u8>,
    ) -> Result<TrackEvent<'a>> {
        let delta = read_varlen(raw).context(err_invalid!("failed to read event deltatime"))?;
        let msg =
            read_message(raw, running_status).context(err_invalid!("failed to parse event"))?;
        Ok(TrackEvent { delta, msg })
    }

    #[inline]
    pub(crate) fn write<W: Write>(
        &self,
        running_status: &mut Option<u8>,
        out: &mut W,
    ) -> WriteResult<W> {
        write_varlen(out, self.delta)?;
        write_message(out, &self.msg, running_status)?;
        Ok(())
    }

    /// Makes this event own its data, cloning any borrowed data if necessary.
    ///
    /// This method is only available with the `alloc` feature enabled.
    #[inline]
    #[cfg(feature = "alloc")]
    pub fn make_owned(&mut self) {
        self.msg.make_owned();
    }

    /// Removes any references to external data by cloning any borrowed data, lifting lifetime
    /// restrictions on this event.
    ///
    /// This method is only available with the `alloc` feature enabled.
    #[inline]
    #[cfg(feature = "alloc")]
    pub fn into_owned(self) -> TrackEvent<'static> {
        TrackEvent {
            delta: self.delta,
            msg: self.msg.into_owned(),
        }
    }

    /// Clears any bytestring data that this event contains.
    #[inline]
    pub fn make_static(&mut self) {
        self.msg.make_static();
    }

    /// Lifts lifetime restrictions on this event by removing bytestring data.
    /// This method will replace any bytestrings by empty bytestrings.
    #[inline]
    pub fn to_static(&self) -> TrackEvent<'static> {
        TrackEvent {
            delta: self.delta,
            msg: self.msg.to_static(),
        }
    }
}

/// Read a single `MidiMessage` following an event deltatime.
#[inline]
fn read_message<'a>(
    raw: &mut &'a mut [u8],
    running_status: &mut Option<u8>,
) -> Result<MidiMessage<'a>> {
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
        let (_, rem) = mem::replace(raw, &mut []).split_at_mut(1);
        *raw = rem;
    }
    //Parse differently based on status
    Ok(match status {
        0x80..=0xEF => {
            *running_status = Some(status);
            let channel = status & 0xF;
            MidiMessage::Channel {
                channel,
                msg: match status >> 4 {
                    0x8 => ChannelMessage::NoteOff {
                        key: read_u8(raw)?,
                        vel: read_u8(raw)?,
                    },
                    0x9 => ChannelMessage::NoteOn {
                        key: read_u8(raw)?,
                        vel: read_u8(raw)?,
                    },
                    0xA => ChannelMessage::Aftertouch {
                        key: read_u8(raw)?,
                        vel: read_u8(raw)?,
                    },
                    0xB => ChannelMessage::Controller {
                        controller: read_u8(raw)?,
                        value: read_u8(raw)?,
                    },
                    0xC => ChannelMessage::ProgramChange {
                        program: read_u8(raw)?,
                    },
                    0xD => ChannelMessage::ChannelAftertouch { vel: read_u8(raw)? },
                    0xE => ChannelMessage::PitchBend {
                        bend: PitchBend::from_u16(
                            read_u8(raw)? as u16 | (read_u8(raw)? as u16) << 7,
                        ),
                    },
                    _ => unreachable!(),
                },
            }
        }
        0xFF => {
            *running_status = None;
            read_meta(raw)?
        }
        0xF0 => {
            *running_status = None;
            read_sysex(raw)?
        }
        0xF7 => {
            *running_status = None;
            MidiMessage::decode(read_varlen_slice(raw)?)
        }
        _ => bail!(err_invalid!("invalid message status")),
    })
}

/// Write a single `MidiMessage` following an event deltatime, using running status if possible.
#[inline]
fn write_message<W: Write>(
    out: &mut W,
    msg: &MidiMessage,
    running_status: &mut Option<u8>,
) -> WriteResult<W> {
    use midly_core::MidiMessage::*;
    //Running Status rules:
    // MIDI Messages (0x80 ..= 0xEF) alter and use running status
    // Any other message cancels and cannot use running status
    let running = *running_status;
    *running_status = None;
    match msg {
        Channel { channel, msg } => {
            use midly_core::ChannelMessage::*;
            let status = base_status(msg) | channel;
            *running_status = Some(status);
            if Some(status) != running {
                //Explicitly write status
                out.write(&[status])?;
            }
            match *msg {
                NoteOff { key, vel } => out.write(&[key, vel]),
                NoteOn { key, vel } => out.write(&[key, vel]),
                Aftertouch { key, vel } => out.write(&[key, vel]),
                Controller { controller, value } => out.write(&[controller, value]),
                ProgramChange { program } => out.write(&[program]),
                ChannelAftertouch { vel } => out.write(&[vel]),
                PitchBend { bend } => {
                    let bend = bend.as_u16();
                    out.write(&[(bend & 0x7F) as u8, (bend >> 7) as u8])
                }
            }
        }

        SysEx(data) => {
            ensure!(
                data.get(0) == Some(&0xF0),
                W::invalid_input("sysex message without a leading F0 byte")
            );
            out.write(&[0xF0])?;
            write_varlen_slice(out, &data[1..])
        }
        &Mtc(kind, val) => write_escape(out, &[0xF1, (kind.as_bits() << 4) | (val & 0xF)]),
        &SongPosition(pos) => write_escape(out, &[0xF2, (pos & 0x7F) as u8, (pos >> 7) as u8]),
        &SongSelect(song) => write_escape(out, &[0xF3, song]),
        TuneRequest => write_escape(out, &[0xF6]),
        TimingClock => write_escape(out, &[0xF8]),
        Start => write_escape(out, &[0xFA]),
        Continue => write_escape(out, &[0xFB]),
        Stop => write_escape(out, &[0xFC]),
        ActiveSensing => write_escape(out, &[0xFE]),
        Reset => write_escape(out, &[0xFF]),
        Unspecified(data) => write_escape(out, data),

        &TrackNumber(track_num) => match track_num {
            None => write_meta(out, 0x00, &[]),
            Some(track_num) => write_meta(out, 0x00, &track_num.to_be_bytes()[..]),
        },
        Text(data) => write_meta(out, 0x01, data),
        Copyright(data) => write_meta(out, 0x02, data),
        TrackName(data) => write_meta(out, 0x03, data),
        InstrumentName(data) => write_meta(out, 0x04, data),
        Lyric(data) => write_meta(out, 0x05, data),
        Marker(data) => write_meta(out, 0x06, data),
        CuePoint(data) => write_meta(out, 0x07, data),
        ProgramName(data) => write_meta(out, 0x08, data),
        DeviceName(data) => write_meta(out, 0x09, data),
        &MidiChannel(chan) => write_meta(out, 0x20, &[chan]),
        &MidiPort(port) => write_meta(out, 0x21, &[port]),
        EndOfTrack => write_meta(out, 0x2F, &[]),
        Tempo(microsperbeat) => write_meta(out, 0x51, &microsperbeat.to_be_bytes()[1..]),
        SmpteOffset(time) => write_meta(out, 0x54, &time.as_bits_full()[..]),
        &TimeSignature(num, den, ticksperclick, thirtysecondsperquarter) => write_meta(
            out,
            0x58,
            &[num, den, ticksperclick, thirtysecondsperquarter],
        ),
        &KeySignature(sharps, minor) => write_meta(out, 0x59, &[sharps as u8, minor as u8]),
        SequencerSpecific(data) => write_meta(out, 0x7F, data),
        UnknownMeta { kind, data } => write_meta(out, *kind, data),
    }
}

/// Get the status byte for the given channel message, if it referenced channel 0.
#[inline]
fn base_status(msg: &ChannelMessage) -> u8 {
    use midly_core::ChannelMessage::*;
    match msg {
        NoteOff { .. } => 0x80,
        NoteOn { .. } => 0x90,
        Aftertouch { .. } => 0xA0,
        Controller { .. } => 0xB0,
        ProgramChange { .. } => 0xC0,
        ChannelAftertouch { .. } => 0xD0,
        PitchBend { .. } => 0xE0,
    }
}

/// Write a single escape message (`0xF7` status byte).
fn write_escape<W: Write>(out: &mut W, data: &[u8]) -> WriteResult<W> {
    out.write(&[0xF7])?;
    write_varlen_slice(out, data)?;
    Ok(())
}

/// Read a single sysex message (`0xF0` status byte).
///
/// This requires some trickery and a mutable source to move bytes around so as to create a
/// contiguous slice of bytes starting with a `0xF0` byte.
fn read_sysex<'a>(raw: &mut &'a mut [u8]) -> Result<MidiMessage<'a>> {
    let (len, varlen_len) = read_varlen_noconsume(*raw)?;
    ensure!(varlen_len >= 1, err_invalid!("no sysex length"));
    advance_slice(raw, varlen_len - 1);
    let data = read_slice(raw, len as usize + 1)?;
    data[0] = 0xF0;
    Ok(MidiMessage::SysEx(data.into()))
}

/// Read a single meta-event, following a `0xFF` status byte.
fn read_meta<'a>(raw: &mut &'a mut [u8]) -> Result<MidiMessage<'a>> {
    let type_byte = read_u8(raw)?;
    let data = read_varlen_slice(raw)?;
    Ok(match type_byte {
        0x00 => MidiMessage::TrackNumber({
            if data.len() >= 2 {
                Some(u16::from_be_bytes([data[0], data[1]]))
            } else {
                None
            }
        }),
        0x01 => MidiMessage::Text(data.into()),
        0x02 => MidiMessage::Copyright(data.into()),
        0x03 => MidiMessage::TrackName(data.into()),
        0x04 => MidiMessage::InstrumentName(data.into()),
        0x05 => MidiMessage::Lyric(data.into()),
        0x06 => MidiMessage::Marker(data.into()),
        0x07 => MidiMessage::CuePoint(data.into()),
        0x08 => MidiMessage::ProgramName(data.into()),
        0x09 => MidiMessage::DeviceName(data.into()),
        0x20 if data.len() >= 1 => MidiMessage::MidiChannel(data[0]),
        0x21 if data.len() >= 1 => MidiMessage::MidiPort(data[0]),
        0x2F => MidiMessage::EndOfTrack,
        0x51 if data.len() >= 3 => {
            MidiMessage::Tempo(u32::from_be_bytes([0, data[0], data[1], data[2]]))
        }
        0x54 if data.len() >= 5 => MidiMessage::SmpteOffset(SmpteTime::from_bits_full(
            data[0], data[1], data[2], data[3], data[4],
        )),
        0x58 if data.len() >= 4 => MidiMessage::TimeSignature(data[0], data[1], data[2], data[3]),
        0x59 if data.len() >= 2 => MidiMessage::KeySignature(data[0] as i8, data[1] != 0),
        0x7F => MidiMessage::SequencerSpecific(data.into()),
        _ => MidiMessage::UnknownMeta {
            kind: type_byte,
            data: data.into(),
        },
    })
}

/// Write a single meta event, including the `0xFF` status byte.
fn write_meta<W: Write>(out: &mut W, kind: u8, data: &[u8]) -> WriteResult<W> {
    out.write(&[0xFF, kind])?;
    write_varlen_slice(out, data)?;
    Ok(())
}
