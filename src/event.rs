use crate::{
    prelude::*,
    primitive::{read_varlen_slice, SmpteTime},
};

/// Represents a fully parsed track event, with delta time.
#[derive(Copy, Clone, Debug)]
pub struct Event<'a> {
    pub delta: u28,
    pub kind: EventKind<'a>,
}
impl<'a> Event<'a> {
    /// The received raw slice should last until the end of the track.
    /// This function will cut down the slice to the appropiate size.
    /// Also, the incoming slice will be advanced to the next event.
    pub fn read(
        raw: &mut &'a [u8],
        running_status: &mut Option<u8>,
    ) -> Result<(&'a [u8], Event<'a>)> {
        let delta = u28::read_u7(raw).context(err_invalid("failed to read event deltatime"))?;
        let (raw, kind) =
            EventKind::read(raw, running_status).context(err_invalid("failed to parse event"))?;
        Ok((raw, Event { delta, kind }))
    }
}

/// Represents the different kinds of events.
#[derive(Copy, Clone, Debug)]
pub enum EventKind<'a> {
    /// A standard MIDI message bound to a channel.
    Midi { channel: u4, message: MidiMessage },
    /// A System Exclusive message, carrying arbitrary data.
    SysEx(&'a [u8]),
    /// An escape sequence, intended to send arbitrary data to the MIDI synthesizer.
    Escape(&'a [u8]),
    /// A meta-message, giving extra information for correct playback, like tempo, song name,
    /// lyrics, etc...
    Meta(MetaMessage<'a>),
}
impl<'a> EventKind<'a> {
    /// Reads a single event from the given stream.
    /// Use this method when reading raw MIDI messages from a stream.
    ///
    /// Returns the event read and the raw bytes that make up the event, taken directly from the
    /// source bytes.
    ///
    /// The running status byte is used to fill in any missing message status (a process known
    /// as "running status").
    /// This running status should be conserved across calls to `EventKind::parse`, and should be
    /// unique per-midi-stream.
    /// Initially it should be set to `None`.
    ///
    /// This method takes a *mutable reference* to a byteslice and a running status byte.
    /// In case of success the byteslice is advanced to the next event, and the running status
    /// might be changed to a new status.
    /// In case of error no changes are made to these values.
    pub fn parse(raw: &mut &'a [u8], running_status: &mut Option<u8>) -> Result<(&'a [u8], EventKind<'a>)> {
        let (old_raw, old_rs) = (*raw, *running_status);
        let maybe_ev = Self::read(raw, running_status);
        if let Err(_) = maybe_ev {
            *raw = old_raw;
            *running_status = old_rs;
        }
        maybe_ev
    }
    
    fn read(
        raw: &mut &'a [u8],
        running_status: &mut Option<u8>,
    ) -> Result<(&'a [u8], EventKind<'a>)> {
        //Keep the beggining of the old slice
        let old_slice = *raw;
        //Read status
        let mut status = *raw.get(0).ok_or(err_invalid("failed to read status"))?;
        if status < 0x80 {
            //Running status!
            status = running_status.ok_or(err_invalid(
                "event missing status with no running status active",
            ))?;
        } else {
            //Set running status
            *running_status = Some(status);
            //Advance slice 1 byte to consume status. Note that because we already did `get()`, we
            //can use panicking index here
            *raw = &raw[1..];
        }
        //Delegate further parsing depending on status
        let kind = match status {
            0x80...0xEF => {
                let channel = u4::from(bit_range(status, 0..4));
                EventKind::Midi {
                    channel,
                    message: MidiMessage::read(raw, status)
                        .context(err_invalid("failed to read midi message"))?,
                }
            }
            0xF0 => EventKind::SysEx(
                read_varlen_slice(raw).context(err_invalid("failed to read sysex event"))?,
            ),
            0xF7 => EventKind::Escape(
                read_varlen_slice(raw).context(err_invalid("failed to read escape event"))?,
            ),
            0xFF => EventKind::Meta(
                MetaMessage::read(raw).context(err_invalid("failed to read meta event"))?,
            ),
            _ => bail!(err_invalid("invalid event status")),
        };
        //The `raw` slice has moved forward by exactly the amount of bytes that form this midi
        //event
        //Therefore the source slice can be determined by rescuing these consumed bytes
        let len = raw.as_ptr() as usize - old_slice.as_ptr() as usize;
        let source_bytes = &old_slice[0..len];
        Ok((source_bytes, kind))
    }
}

/// Represents a MIDI message, not an event.
///
/// If reading a MIDI message from some stream, use `EventKind::read` instead and discard non-midi
/// events.
/// This is the correct way to handle running status.
#[derive(Copy, Clone, Debug)]
pub enum MidiMessage {
    /// Stop playing a note.
    NoteOff {
        /// The MIDI key to stop playing.
        key: u7,
        /// The velocity with which to stop playing it.
        vel: u7
    },
    /// Start playing a note.
    NoteOn {
        /// The key to start playing.
        key: u7,
        /// The velocity (strength) with which to press it.
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
        /// The new velocity for the notes currently playing in the channel.
        vel: u7,
    },
    /// Set the pitch bend value.
    PitchBend {
        /// The new pitch-bend value.
        ///
        /// A value of `0x0000` indicates full bend downwards.
        /// A value of `0x2000` indicates no bend.
        /// A value of `0x3FFF` indicates full bend upwards.
        bend: u14,
    },
}
impl MidiMessage {
    /// Receives a slice pointing to midi args (not including status byte)
    /// Status byte is given separately to reuse running status
    fn read(raw: &mut &[u8], status: u8) -> Result<MidiMessage> {
        Ok(match bit_range(status, 4..8) {
            0x8 => MidiMessage::NoteOff{key: u7::read(raw)?, vel:u7::read(raw)?},
            0x9 => MidiMessage::NoteOn{key: u7::read(raw)?, vel: u7::read(raw)?},
            0xA => MidiMessage::Aftertouch{key: u7::read(raw)?, vel: u7::read(raw)?},
            0xB => MidiMessage::Controller{controller: u7::read(raw)?, value: u7::read(raw)?},
            0xC => MidiMessage::ProgramChange{program: u7::read(raw)?},
            0xD => MidiMessage::ChannelAftertouch{vel: u7::read(raw)?},
            0xE => MidiMessage::PitchBend{bend: u14::read_u7(raw)?},
            _ => bail!(err_invalid("invalid midi message status")),
        })
    }
}

#[derive(Copy, Clone, Debug)]
pub enum MetaMessage<'a> {
    /// For `Format::Sequential` MIDI file types, `TrackNumber` can be empty, and defaults to
    /// track index.
    TrackNumber(Option<u16>),
    Text(&'a [u8]),
    Copyright(&'a [u8]),
    TrackName(&'a [u8]),
    InstrumentName(&'a [u8]),
    Lyric(&'a [u8]),
    Marker(&'a [u8]),
    CuePoint(&'a [u8]),
    ProgramName(&'a [u8]),
    DeviceName(&'a [u8]),
    MidiChannel(u4),
    MidiPort(u7),
    /// Obligatory at track end.
    EndOfTrack,
    /// Amount of microseconds per beat (quarter note).
    ///
    /// Usually appears at the beggining of a track, before any midi events are sent, but there
    /// are no guarantees.
    Tempo(u24),
    SmpteOffset(SmpteTime),
    /// In order of the MIDI specification, numerator, denominator, midi clocks per click, 32nd
    /// notes per quarter
    TimeSignature(u8, u8, u8, u8),
    /// As in the MIDI specification, negative numbers indicate number of flats, positive number
    /// of sharps `false` indicates major, `true` indicates minor.
    KeySignature(i8, bool),
    SequencerSpecific(&'a [u8]),
    /// An unknown meta-message, unconforming to the spec.
    ///
    /// This event is not generated with the `strict` feature enabled.
    Unknown(u8, &'a [u8]),
}
impl<'a> MetaMessage<'a> {
    fn read(raw: &mut &'a [u8]) -> Result<MetaMessage<'a>> {
        let type_byte = u8::read(raw).context(err_invalid("failed to read meta message type"))?;
        let mut data =
            read_varlen_slice(raw).context(err_invalid("failed to read meta message data"))?;
        Ok(match type_byte {
            0x00 => MetaMessage::TrackNumber({
                if cfg!(feature = "strict") {
                    ensure!(
                        data.len() == 0 || data.len() == 2,
                        err_pedantic("invalid tracknumber event length")
                    );
                }
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
            0x20 if data.len() >= 1 => {
                if cfg!(feature = "strict") {
                    ensure!(
                        data.len() == 1,
                        err_pedantic("invalid midichannel event length")
                    );
                }
                MetaMessage::MidiChannel(u4::read(&mut data)?)
            }
            0x21 if data.len() >= 1 => {
                if cfg!(feature = "strict") {
                    ensure!(
                        data.len() == 1,
                        err_pedantic("invalid midiport event length")
                    );
                }
                MetaMessage::MidiPort(u7::read(&mut data)?)
            }
            0x2F => {
                if cfg!(feature = "strict") {
                    ensure!(
                        data.len() == 0,
                        err_pedantic("invalid endoftrack event length")
                    );
                }
                MetaMessage::EndOfTrack
            }
            0x51 if data.len() >= 3 => {
                if cfg!(feature = "strict") {
                    ensure!(data.len() == 3, err_pedantic("invalid tempo event length"));
                }
                MetaMessage::Tempo(u24::read(&mut data)?)
            }
            0x54 if data.len() >= 5 => {
                if cfg!(feature = "strict") {
                    ensure!(
                        data.len() == 5,
                        err_pedantic("invalid smpteoffset event length")
                    );
                }
                MetaMessage::SmpteOffset(
                    SmpteTime::read(&mut data).context(err_invalid("failed to read smpte time"))?,
                )
            }
            0x58 if data.len() >= 4 => {
                if cfg!(feature = "strict") {
                    ensure!(
                        data.len() == 4,
                        err_pedantic("invalid timesignature event length")
                    );
                }
                MetaMessage::TimeSignature(
                    u8::read(&mut data)?,
                    u8::read(&mut data)?,
                    u8::read(&mut data)?,
                    u8::read(&mut data)?,
                )
            }
            0x59 => {
                MetaMessage::KeySignature(u8::read(&mut data)? as i8, u8::read(&mut data)? != 0)
            }
            0x7F => MetaMessage::SequencerSpecific(data),
            _ => {
                if cfg!(feature = "strict") {
                    bail!(err_pedantic("unknown meta event type"))
                } else {
                    MetaMessage::Unknown(type_byte, data)
                }
            }
        })
    }
}
