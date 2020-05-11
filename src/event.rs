//! All sort of events and their parsers.

use crate::{
    prelude::*,
    primitive::{read_varlen_slice, write_varlen_slice, SmpteTime},
};

/// Represents a parsed SMF track event.
///
/// Consists of a delta time with respect to the previous event and the actual MIDI event.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Event<'a> {
    /// How many MIDI ticks after the previous event should this event fire off.
    pub delta: u28,
    /// The type of event along with event-specific data.
    pub kind: EventKind<'a>,
}
impl<'a> Event<'a> {
    /// Read an `Smf` track event from raw track data.
    ///
    /// The first argument is a mutable reference to a raw byte slice: the track to parse.
    /// The bytes corresponding to this event are removed from the slice by advancing it to the
    /// next event.
    ///
    /// The second argument is a mutable reference to the current "running status".
    /// Running status allows consecutive events to share their status if there is no status change,
    /// so running status should be shared across calls to `Event::read`.
    pub(crate) fn read(raw: &mut &'a [u8], running_status: &mut Option<u8>) -> Result<Event<'a>> {
        let delta = u28::read_u7(raw).context(err_invalid!("failed to read event deltatime"))?;
        let kind =
            EventKind::read(raw, running_status).context(err_invalid!("failed to parse event"))?;
        Ok(Event { delta, kind })
    }

    pub(crate) fn read_bytemap(
        raw: &mut &'a [u8],
        running_status: &mut Option<u8>,
    ) -> Result<(&'a [u8], Event<'a>)> {
        let delta = u28::read_u7(raw).context(err_invalid!("failed to read event deltatime"))?;
        let old_raw = *raw;
        let kind =
            EventKind::read(raw, running_status).context(err_invalid!("failed to parse event"))?;
        Ok((&old_raw[..old_raw.len() - raw.len()], Event { delta, kind }))
    }

    pub(crate) fn write<W: Write>(
        &self,
        running_status: &mut Option<u8>,
        out: &mut W,
    ) -> IoResult<W> {
        self.delta.write_varlen(out)?;
        self.kind.write(running_status, out)?;
        Ok(())
    }
}

/// Represents the different kinds of SMF events and their associated data.
///
/// It notably does *not* include the timing of the event, the `Event` struct is responsible for
/// this.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum EventKind<'a> {
    /// A standard MIDI message bound to a channel.
    Midi { channel: u4, message: MidiMessage },
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
impl<'a> EventKind<'a> {
    fn read(raw: &mut &'a [u8], running_status: &mut Option<u8>) -> Result<EventKind<'a>> {
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
                EventKind::Midi { channel, message }
            }
            0xFF => {
                *running_status = None;
                EventKind::Meta(
                    MetaMessage::read(raw).context(err_invalid!("failed to read meta event"))?,
                )
            }
            0xF0 => {
                *running_status = None;
                EventKind::SysEx(
                    read_varlen_slice(raw).context(err_invalid!("failed to read sysex event"))?,
                )
            }
            0xF7 => {
                *running_status = None;
                EventKind::Escape(
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
    ///
    /// If you wish to disable running status, pass in `&mut None` to all calls to this method.
    pub fn write<W: Write>(&self, running_status: &mut Option<u8>, out: &mut W) -> IoResult<W> {
        //Running Status rules:
        // - MIDI Messages (0x80 ..= 0xEF) alter and use running status
        // - System Common (0xF0 ..= 0xF7) cancel and cannot use running status
        // - System Realtime (0xF8 ..= 0xFE) do not alter running status and cannot use it either
        // - Meta Messages (0xFF) cancel and cannot use running status
        match self {
            EventKind::Midi { channel, message } => {
                let status = message.status_nibble() << 4 | channel.as_int();
                if Some(status) != *running_status {
                    //Explicitly write status
                    out.write(&[status])?;
                    *running_status = Some(status);
                }
                message.write(out)?;
            }
            EventKind::SysEx(data) => {
                *running_status = None;
                out.write(&[0xF0])?;
                write_varlen_slice(data, out)?;
            }
            EventKind::Escape(data) => {
                *running_status = None;
                out.write(&[0xF7])?;
                write_varlen_slice(data, out)?;
            }
            EventKind::Meta(meta) => {
                out.write(&[0xFF])?;
                meta.write(out)?;
            }
        }
        Ok(())
    }
}

/// Represents a MIDI message, not an event.
///
/// If reading a MIDI message from some stream, use `EventKind::read` instead and discard non-midi
/// events.
/// This is the correct way to handle running status.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
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
    /// Midi messages have a known length.
    const LENGTH_BY_STATUS: [u8; 16] = [0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 1, 1, 2, 0];

    fn msg_length(status: u8) -> usize {
        Self::LENGTH_BY_STATUS[(status >> 4) as usize] as usize
    }

    /// Extract the data bytes from a raw slice.
    fn read_data_u8(status: u8, raw: &mut &[u8]) -> Result<[u7; 2]> {
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
    fn get_data_u7(status: u8, data: &[u7]) -> Result<[u7; 2]> {
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
    fn read(status: u8, data: [u7; 2]) -> (u4, MidiMessage) {
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
                    bend: u14::from(msb << 7 | lsb),
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
    fn write<W: Write>(&self, out: &mut W) -> IoResult<W> {
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
                out.write(&[(bend.as_int() & 0x7F) as u8, (bend.as_int() >> 7) as u8])?
            }
        }
        Ok(())
    }
}

/// A streaming raw MIDI parser.
/// This parser takes raw MIDI, *not* `.midi` files!
///
/// `MidiStream` allows for stream-based parsing,
pub struct MidiStream {
    status: Option<u8>,
    data: Vec<u7>,
}
impl MidiStream {
    pub fn new() -> MidiStream {
        MidiStream {
            status: None,
            data: Vec::new(),
        }
    }

    fn event(&mut self, status: u8, handle_ev: &mut dyn FnMut(StreamEvent)) {
        if let Ok(ev) = StreamEvent::read(status, &self.data[..]) {
            handle_ev(ev);
        }
    }

    fn feed_byte(&mut self, byte: u8, handle_ev: &mut dyn FnMut(StreamEvent)) {
        if let Some(byte) = u7::try_from(byte) {
            //Data byte
            if let Some(status) = self.status {
                //Midi messages have a known length, so when a data byte beyond the fixed message
                //length arrives, we must know to finish off the previous message and start a new
                //one sharing the status of the previous byte (running status).
                let len = MidiMessage::msg_length(status);
                if len > 0 && self.data.len() >= len {
                    self.event(status, handle_ev);
                    self.data.clear();
                }
                self.data.push(byte);
            }
        } else {
            //Status byte
            if let 0xF8..=0xFF = byte {
                //System Realtime
                //These single-byte events are intended to transmit quick time-sensitive events,
                //and they should be invisible to other messages (that means, they don't alter any
                //decoder state).
                //They can appear in between the status and data bytes of other messages, and even
                //in between the data bytes of other messages.
                handle_ev(StreamEvent::Realtime(SystemRealtime::read(byte)));
            } else {
                //Channel/System Common
                //Because the status is about to be cleared, process the previous one
                if let Some(status) = self.status {
                    self.event(status, handle_ev);
                }
                //Set the new status
                self.status = Some(byte);
                self.data.clear();
            }
        }
    }

    fn feed_impl(&mut self, bytes: &[u8], handle_ev: &mut dyn FnMut(StreamEvent)) {
        for &byte in bytes {
            self.feed_byte(byte, handle_ev);
        }
    }

    pub fn feed(&mut self, bytes: &[u8], mut handle_ev: impl FnMut(StreamEvent)) {
        self.feed_impl(bytes, &mut handle_ev)
    }

    fn flush_impl(&mut self, handle_ev: &mut dyn FnMut(StreamEvent)) {
        if let Some(status) = self.status.take() {
            self.event(status, handle_ev);
            self.status = None;
            self.data.clear();
        }
    }

    pub fn flush(&mut self, mut handle_ev: impl FnMut(StreamEvent)) {
        self.flush_impl(&mut handle_ev);
    }

    /// Equivalent to a call to `feed` followed by a call to `flush`.
    pub fn parse(&mut self, bytes: &[u8], mut handle_ev: impl FnMut(StreamEvent)) {
        self.feed_impl(bytes, &mut handle_ev);
        self.flush_impl(&mut handle_ev);
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum StreamEvent<'a> {
    Midi { channel: u4, message: MidiMessage },
    Common(SystemCommon<'a>),
    Realtime(SystemRealtime),
}
impl<'a> StreamEvent<'a> {
    /// Parse a complete MIDI message from its raw bytes.
    pub fn parse(mut raw: &'a [u8]) -> Result<StreamEvent<'a>> {
        let status = raw
            .split_checked(1)
            .ok_or_else(|| err_invalid!("no status byte"))?[0];
        let data = u7::from_int_slice(raw);
        Self::read(status, data)
    }

    fn read(status: u8, data: &[u7]) -> Result<StreamEvent> {
        match status {
            0x80..=0xEF => {
                let data = MidiMessage::get_data_u7(status, data)?;
                let (channel, message) = MidiMessage::read(status, data);
                Ok(StreamEvent::Midi { channel, message })
            }
            _ => {
                //System Common event
                let ev = SystemCommon::read(status, data)?;
                Ok(StreamEvent::Common(ev))
            }
        }
    }

    /// Write the complete message, including status.
    pub fn write<W: Write>(&self, out: &mut W) -> IoResult<W> {
        match self {
            StreamEvent::Midi { channel, message } => {
                let status = message.status_nibble() << 4 | channel.as_int();
                out.write(&[status])?;
                message.write(out)?;
            }
            StreamEvent::Common(common) => {
                common.write(out)?;
            }
            StreamEvent::Realtime(realtime) => {
                out.write(&[realtime.encode()])?;
            }
        }
        Ok(())
    }
}

/// Messages that only occur in live MIDI connections (ie. not SMF), and can occur at ANY time,
/// even during transmission of other event data bytes.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum SystemRealtime {
    /// If sent, they should be sent 24 times per quarter note.
    TimingClock,
    Start,
    Continue,
    Stop,
    /// Once one of these messages is transmitted, a message should arrive every 300ms or else the
    /// connection is considered broken.
    ActiveSensing,
    Reset,
    Undefined(u8),
}
impl SystemRealtime {
    pub(crate) fn read(status: u8) -> SystemRealtime {
        use SystemRealtime::*;
        match status {
            0xF8 => TimingClock,
            0xFA => Start,
            0xFB => Continue,
            0xFC => Stop,
            0xFE => ActiveSensing,
            0xFF => Reset,
            _ => {
                //Unknown system realtime event
                Undefined(status)
            }
        }
    }

    fn encode(self) -> u8 {
        use SystemRealtime::*;
        match self {
            TimingClock => 0xF8,
            Start => 0xFA,
            Continue => 0xFB,
            Stop => 0xFC,
            ActiveSensing => 0xFE,
            Reset => 0xFF,
            Undefined(byte) => byte,
        }
    }
}

/// A "system common event", as defined by the MIDI spec.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum SystemCommon<'a> {
    /// A system-exclusive event. The data does not include the leading `F0` or the closing `F7`,
    /// only data bytes are allowed.
    SysEx(&'a [u7]),
    MidiTimeCodeQuarterFrame(MtcQuarterFrameMessage, u4),
    /// The number of MIDI beats (6 x MIDI clocks) that have elapsed since the start of the
    /// sequence.
    SongPosition(u14),
    SongSelect(u7),
    TuneRequest,
    /// An undefined System Common message, with arbitrary data bytes.
    Undefined(u8, &'a [u7]),
}
impl<'a> SystemCommon<'a> {
    fn read(status: u8, data: &'a [u7]) -> Result<SystemCommon<'a>> {
        let ev = match status {
            0xF0 => {
                //SysEx
                SystemCommon::SysEx(&data[..])
            }
            0xF1 if data.len() >= 1 => {
                //MTC Quarter Frame
                SystemCommon::MidiTimeCodeQuarterFrame(
                    MtcQuarterFrameMessage::from_code(data[0].as_int() >> 4).unwrap(),
                    u4::from(data[0].as_int()),
                )
            }
            0xF2 if data.len() >= 2 => {
                //Song Position
                SystemCommon::SongPosition(u14::from(
                    (data[0].as_int() as u16) | ((data[1].as_int() as u16) << 7),
                ))
            }
            0xF3 if data.len() >= 1 => {
                //Song Select
                SystemCommon::SongSelect(u7::from(data[0]))
            }
            0xF6 => {
                //Tune Request
                SystemCommon::TuneRequest
            }
            0xF1..=0xF5 => {
                //Unknown system common event
                SystemCommon::Undefined(status, &data[..])
            }
            _ => {
                //Invalid/Unknown/Unreachable event
                //(Including F7 SysEx End Marker)
                bail!(err_invalid!("invalid status byte"))
            }
        };
        Ok(ev)
    }

    fn write<W: Write>(&self, out: &mut W) -> IoResult<W> {
        match self {
            SystemCommon::SysEx(data) => {
                out.write(&[0xF0])?;
                out.write(u7::as_int_slice(data))?;
                out.write(&[0xF7])
            }
            SystemCommon::MidiTimeCodeQuarterFrame(msgtype, data) => {
                out.write(&[0xF1, msgtype.as_code() << 4 | data.as_int()])
            }
            SystemCommon::SongPosition(pos) => {
                out.write(&[0xF2, pos.as_int() as u8 & 0x7F, (pos.as_int() >> 7) as u8])
            }
            SystemCommon::SongSelect(song) => out.write(&[0xF3, song.as_int()]),
            SystemCommon::TuneRequest => out.write(&[0xF6]),
            SystemCommon::Undefined(status, data) => {
                out.write(&[*status])?;
                out.write(u7::as_int_slice(data))
            }
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum MtcQuarterFrameMessage {
    FramesLow,
    FramesHigh,
    SecondsLow,
    SecondsHigh,
    MinutesLow,
    MinutesHigh,
    HoursLow,
    HoursHigh,
}
impl MtcQuarterFrameMessage {
    fn as_code(self) -> u8 {
        use MtcQuarterFrameMessage::*;
        match self {
            FramesLow => 0,
            FramesHigh => 1,
            SecondsLow => 2,
            SecondsHigh => 3,
            MinutesLow => 4,
            MinutesHigh => 5,
            HoursLow => 6,
            HoursHigh => 7,
        }
    }
    fn from_code(code: u8) -> Option<Self> {
        use MtcQuarterFrameMessage::*;
        Some(match code {
            0 => FramesLow,
            1 => FramesHigh,
            2 => SecondsLow,
            3 => SecondsHigh,
            4 => MinutesLow,
            5 => MinutesHigh,
            6 => HoursLow,
            7 => HoursHigh,
            _ => return None,
        })
    }
}

/// A "meta message", as defined by the SMF spec.
/// These events carry metadata about the track, such as tempo, time signature, copyright, etc...
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum MetaMessage<'a> {
    /// For `Format::Sequential` MIDI file types, `TrackNumber` can be empty, and defaults to
    /// the track index.
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
    /// In order of the MIDI specification, numerator, denominator, MIDI clocks per click, 32nd
    /// notes per quarter
    TimeSignature(u8, u8, u8, u8),
    /// As in the MIDI specification, negative numbers indicate number of flats and positive
    /// numbers indicate number of sharps.
    /// `false` indicates a major scale, `true` indicates a minor scale.
    KeySignature(i8, bool),
    SequencerSpecific(&'a [u8]),
    /// An unknown or malformed meta-message.
    ///
    /// The first `u8` is the raw meta-message identifier byte.
    /// The slice is the actual payload of the meta-message.
    Unknown(u8, &'a [u8]),
}
impl<'a> MetaMessage<'a> {
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
    fn write<W: Write>(&self, out: &mut W) -> IoResult<W> {
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
