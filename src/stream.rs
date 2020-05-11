//! Provides a way to read and write "live" MIDI messages for example produced by real instruments,
//! in contrast with "dead" MIDI messages that are stored in `.mid` Standard Midi Files.
//!
//! [`Event`](../struct.Event.html)s parsed from a `.mid` SMF file differ from on-the-wire events
//! such as those produced by a real instrument.
//!
//! There are two flavors of live events:
//!
//! - "Packet-like" events, which consist of a single buffer of bytes that represents a complete
//!     MIDI message.
//!     This type of event is produced by almost all OS midi APIs, and is the type of event that
//!     `midir` produces.
//!
//!     Handling these events is done through the
//!     [`parse`](struct.LiveEvent.html#method.parse),
//!     [`write`](struct.LiveEvent.html#method.write)
//!     and [`write_with_running_status`](struct.LiveEvent.html#method.write_with_running_status)
//!     methods on the `LiveEvent` type.
//!
//! - "Stream-like" events, which consist of an uninterrupted stream of bytes, with no delimiter
//!     in between messages.
//!     This type of stream is extremely low-level, and it's rare to come across such a stream.
//!     This kind of stream could be seen in low-level drivers or embedded hardware with direct
//!     access to a MIDI stream.
//!
//!     Reading these streams is done through the [`MidiStream`](struct.MidiStream.html) type, and
//!     writing these streams is done in the same way that "packet-like" events are written.

use crate::{event::MidiMessage, prelude::*};

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum LiveEvent<'a> {
    Midi { channel: u4, message: MidiMessage },
    Common(SystemCommon<'a>),
    Realtime(SystemRealtime),
}
impl<'a> LiveEvent<'a> {
    /// Parse a complete MIDI message from its raw bytes.
    pub fn parse(mut raw: &'a [u8]) -> Result<LiveEvent<'a>> {
        let status = raw
            .split_checked(1)
            .ok_or_else(|| err_invalid!("no status byte"))?[0];
        let data = u7::from_int_slice(raw);
        Self::read(status, data)
    }

    fn read(status: u8, data: &[u7]) -> Result<LiveEvent> {
        match status {
            0x80..=0xEF => {
                let data = MidiMessage::get_data_u7(status, data)?;
                let (channel, message) = MidiMessage::read(status, data);
                Ok(LiveEvent::Midi { channel, message })
            }
            _ => {
                //System Common event
                let ev = SystemCommon::read(status, data)?;
                Ok(LiveEvent::Common(ev))
            }
        }
    }

    /// Write a standalone message to the given output.
    pub fn write<W: Write>(&self, out: &mut W) -> IoResult<W> {
        self.write_with_running_status(&mut None, out)
    }

    /// Write a message, skipping the status if it shares the status with the previous message.
    pub fn write_with_running_status<W: Write>(
        &self,
        running_status: &mut Option<u8>,
        out: &mut W,
    ) -> IoResult<W> {
        match self {
            LiveEvent::Midi { channel, message } => {
                let status = message.status_nibble() << 4 | channel.as_int();
                if Some(status) != *running_status {
                    *running_status = Some(status);
                    out.write(&[status])?;
                }
                message.write(out)?;
            }
            LiveEvent::Common(common) => {
                *running_status = None;
                common.write(out)?;
            }
            LiveEvent::Realtime(realtime) => {
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
    pub fn new(status: u8) -> SystemRealtime {
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

    pub fn encode(self) -> u8 {
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

    fn event(&mut self, status: u8, mut handle_ev: impl FnMut(LiveEvent)) {
        if let Ok(ev) = LiveEvent::read(status, &self.data[..]) {
            handle_ev(ev);
        }
    }

    fn feed_byte(&mut self, byte: u8, mut handle_ev: impl FnMut(LiveEvent)) {
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
                handle_ev(LiveEvent::Realtime(SystemRealtime::new(byte)));
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

    pub fn feed(&mut self, bytes: &[u8], mut handle_ev: impl FnMut(LiveEvent)) {
        for &byte in bytes {
            self.feed_byte(byte, &mut handle_ev);
        }
    }

    pub fn flush(&mut self, handle_ev: impl FnMut(LiveEvent)) {
        if let Some(status) = self.status.take() {
            self.event(status, handle_ev);
            self.status = None;
            self.data.clear();
        }
    }
}
