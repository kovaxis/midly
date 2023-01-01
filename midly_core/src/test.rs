use crate::{MidiMessage, MtcKind};

fn test_equiv(list: &[(&[u8], MidiMessage)]) {
    for (raw, msg) in list {
        let mut buf = [0; 3];
        assert_eq!(&MidiMessage::decode(raw), msg);
        assert_eq!(msg.encode(&mut buf), *raw);
    }
}

#[test]
fn channel_msg() {
    use crate::{ChannelMessage::*, MidiMessage::Channel};
    test_equiv(&[
        // Note off
        (
            &[0x80, 0, 63],
            Channel {
                channel: 0,
                msg: NoteOff { key: 0, vel: 63 },
            },
        ),
        (
            &[0x87, 121, 127],
            Channel {
                channel: 7,
                msg: NoteOff { key: 121, vel: 127 },
            },
        ),
        (
            &[0x8A, 127, 0],
            Channel {
                channel: 10,
                msg: NoteOff { key: 127, vel: 0 },
            },
        ),
        // Note on
        (
            &[0x90, 0, 63],
            Channel {
                channel: 0,
                msg: NoteOn { key: 0, vel: 63 },
            },
        ),
        (
            &[0x97, 121, 127],
            Channel {
                channel: 7,
                msg: NoteOn { key: 121, vel: 127 },
            },
        ),
        (
            &[0x9A, 127, 0],
            Channel {
                channel: 10,
                msg: NoteOn { key: 127, vel: 0 },
            },
        ),
        // Aftertouch
        (
            &[0xA0, 0, 63],
            Channel {
                channel: 0,
                msg: Aftertouch { key: 0, vel: 63 },
            },
        ),
        (
            &[0xA7, 121, 127],
            Channel {
                channel: 7,
                msg: Aftertouch { key: 121, vel: 127 },
            },
        ),
        (
            &[0xAA, 127, 0],
            Channel {
                channel: 10,
                msg: Aftertouch { key: 127, vel: 0 },
            },
        ),
        // Controller change
        (
            &[0xB0, 0, 63],
            Channel {
                channel: 0,
                msg: Controller {
                    controller: 0,
                    value: 63,
                },
            },
        ),
        (
            &[0xB7, 121, 127],
            Channel {
                channel: 7,
                msg: Controller {
                    controller: 121,
                    value: 127,
                },
            },
        ),
        (
            &[0xBA, 127, 0],
            Channel {
                channel: 10,
                msg: Controller {
                    controller: 127,
                    value: 0,
                },
            },
        ),
        // Program change
        (
            &[0xC0, 0],
            Channel {
                channel: 0,
                msg: ProgramChange { program: 0 },
            },
        ),
        (
            &[0xC7, 121],
            Channel {
                channel: 7,
                msg: ProgramChange { program: 121 },
            },
        ),
        (
            &[0xCA, 127],
            Channel {
                channel: 10,
                msg: ProgramChange { program: 127 },
            },
        ),
        // Channel aftertouch
        (
            &[0xD0, 0],
            Channel {
                channel: 0,
                msg: ChannelAftertouch { vel: 0 },
            },
        ),
        (
            &[0xD7, 121],
            Channel {
                channel: 7,
                msg: ChannelAftertouch { vel: 121 },
            },
        ),
        (
            &[0xDA, 127],
            Channel {
                channel: 10,
                msg: ChannelAftertouch { vel: 127 },
            },
        ),
        // Pitch bend
        (
            &[0xE0, 0, 0],
            Channel {
                channel: 0,
                msg: PitchBend {
                    bend: crate::PitchBend::from_u16(0),
                },
            },
        ),
        (
            &[0xE0, 127, 0],
            Channel {
                channel: 0,
                msg: PitchBend {
                    bend: crate::PitchBend::from_u16(127),
                },
            },
        ),
        (
            &[0xE0, 0, 0],
            Channel {
                channel: 0,
                msg: PitchBend {
                    bend: crate::PitchBend::from_i16(-0x2000),
                },
            },
        ),
        (
            &[0xE0, 0, 64],
            Channel {
                channel: 0,
                msg: PitchBend {
                    bend: crate::PitchBend::from_i16(0),
                },
            },
        ),
        (
            &[0xE0, 127, 127],
            Channel {
                channel: 0,
                msg: PitchBend {
                    bend: crate::PitchBend::from_i16(0x1FFF),
                },
            },
        ),
    ]);
}

#[test]
fn system_msg() {
    use crate::MidiMessage::*;
    test_equiv(&[
        // System common
        (
            &[0xF0, b'h', b'e', b'l', b'l', b'o', 0xF7],
            SysEx(b"\xF0hello\xF7"[..].into()),
        ),
        (&[0xF1, 0x36], Mtc(MtcKind::SecondsHi, 6)),
        (&[0xF2, 0x08, 0x01], SongPosition(136)),
        (&[0xF3, 0x01], SongSelect(1.into())),
        (&[0xF4], Unspecified([0xF4][..].into())),
        (&[0xF5], Unspecified([0xF5][..].into())),
        (&[0xF6], TuneRequest),
        (&[0xF7], Unspecified([0xF7][..].into())),
        // System realtime
        (&[0xF8], TimingClock),
        (&[0xF9], Unspecified([0xF9][..].into())),
        (&[0xFA], Start),
        (&[0xFB], Continue),
        (&[0xFC], Stop),
        (&[0xFD], Unspecified([0xFD][..].into())),
        (&[0xFE], ActiveSensing),
        (&[0xFF], Reset),
    ]);
}
