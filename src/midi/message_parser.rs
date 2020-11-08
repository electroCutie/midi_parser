use super::parsed_message::*;
use super::small_ints::*;

fn parse_voice_arr<'a>(msg: &[u8; 4]) -> Option<VoiceMessage> {
    let chan: Channel = Channel::mask(msg[1] & 0x0f);
    let b1: U7 = U7::mask(msg[2]);
    let b2: U7 = U7::mask(msg[3]);

    match msg[1] >> 4 {
        0x08 => Some(VoiceMessage::NoteOff(chan, b1.into(), b2.into())),
        0x09 => Some(match b2.into() {
            0u8 => VoiceMessage::NoteOff(chan, b1.into(), Velocity::mask(0x40)), // Note on with 0 is really an off
            _ => VoiceMessage::NoteOn(chan, b1.into(), b2.into()),
        }),
        0x0a => Some(VoiceMessage::Pressure(chan, b1.into(), b2.into())),
        0x0b => {
            if u8::from(b1) < 0x78u8 || u8::from(b1) >= 0x80u8 {
                Some(VoiceMessage::Controller(chan, b1.into(), b2.into()))
            } else {
                // 0x79-0x7f are actually Channel mode messages
                None
            }
        }
        0x0c => Some(VoiceMessage::ProgramChange(chan, b1.into())),
        0x0d => Some(VoiceMessage::ChannelPressure(chan, b1.into())),
        0x0e => Some(VoiceMessage::PitchBend(chan, U14::from_u7s(b2, b1).into())),
        _ => None,
    }
}

#[cfg(test)]
mod test_voice {
    use super::*;

    macro_rules! voice_test {
        ($name: ident, $bytes: tt, $cons: ident, $chan: literal, $first: literal $(, $heck: ident, $second: literal)? ) => {
            #[test]
            fn $name (){
                let v = parse_voice_arr(&$bytes).unwrap();
                match v {
                    VoiceMessage::$cons(c, n $(, $heck)?  ) => {
                        assert_eq!($chan, u8::from(c));
                        assert_eq!($first, n.into());
                        $( assert_eq!($second, $heck.into());)?
                    }
                    x => panic!("wrong message type {:?}", x),
                }
            }
        };
    }

    #[rustfmt::skip] voice_test!(note_on, [0x00, 0x91, 0x3c, 0x44], NoteOn, 1, 0x3Cu8, h, 0x44u8);
    #[rustfmt::skip] voice_test!(note_on_0_velocity, [0x00, 0x91, 0x3c, 0x00], NoteOff, 1, 0x3Cu8, h, 0x40u8);
    #[rustfmt::skip] voice_test!(note_off, [0x00, 0x82, 0x3c, 0x30], NoteOff, 2, 0x3Cu8, h, 0x30u8);
    #[rustfmt::skip] voice_test!(poly_pressure, [0x00, 0xA2, 0x3c, 0x35], Pressure, 2, 0x3Cu8, h, 0x35u8);
    #[rustfmt::skip] voice_test!(controller, [0x00, 0xB2, 0x77, 0x35], Controller, 2, 0x77u8, h, 0x35u8);
    #[rustfmt::skip] voice_test!(pressure, [0x00, 0xB2, 0x77, 0x35], Controller, 2, 0x77u8, h, 0x35u8);

    #[rustfmt::skip] voice_test!(program, [0x00, 0xC2, 0x14, 0x00], ProgramChange, 2, 0x14u8);
    #[rustfmt::skip] voice_test!(channel_pressure, [0x00, 0xD2, 0x14, 0x00], ChannelPressure, 2, 0x14u8);

    #[rustfmt::skip] voice_test!(bend, [0x00, 0xE2, 0b0101010, 0b01111111], PitchBend, 2, 0b11111110101010u16);

    #[test]
    fn controller_as_channel() {
        assert!(parse_voice_arr(&[0x00, 0xB2, 0x78, 0x35]).is_none());
    }
}

fn parse_channel_arr(msg: &[u8; 4]) -> Option<ChannelMessage> {
    let chan: Channel = Channel::mask(msg[1] & 0x0f);
    if (msg[1] >> 4) != 0xb {
        return None;
    }

    match msg[2] {
        0x78 => Some(ChannelMessage::SoundOff(chan)),
        0x79 => Some(ChannelMessage::Reset(chan)),
        0x7a => Some(ChannelMessage::ChangeControl(
            chan,
            match msg[3] {
                0 => LocalControl::NoLocalControl,
                _ => LocalControl::ControlLocally,
            },
        )),
        0x7b => Some(ChannelMessage::NotesOff(chan)),
        0x7c => Some(ChannelMessage::OmniOff(chan)),
        0x7d => Some(ChannelMessage::OmniOn(chan)),
        0x7e => Some(ChannelMessage::MonoMode(chan)),
        0x7f => Some(ChannelMessage::PolyMode(chan)),
        _ => None,
    }
}

#[cfg(test)]
mod test_channel {
    use super::super::parsed_message::LocalControl;
    use super::*;

    macro_rules! channel_test {
        ($name: ident, $bytes: tt, $cons: ident, $chan: literal $(, $heck: ident, $first: path)? ) => {
            #[test]
            fn $name (){
                let v = parse_channel_arr(&$bytes).unwrap();
                match v {
                    ChannelMessage::$cons(c $(, $heck)?  ) => {
                        assert_eq!($chan, u8::from(c));
                        $( assert_eq!($first, $heck.into());)?
                    }
                    x => panic!("wrong message type {:?}", x),
                }
            }
        };
    }

    channel_test!(sound_off, [0x00, 0xB1, 0x78, 0x00], SoundOff, 1);
    channel_test!(reset, [0x00, 0xB1, 0x79, 0x00], Reset, 1);

    channel_test!(
        local_control_on,
        [0x00, 0xB1, 0x7A, 0x7F],
        ChangeControl,
        1,
        h,
        LocalControl::ControlLocally
    );
    channel_test!(
        local_control_off,
        [0x00, 0xB1, 0x7A, 0x00],
        ChangeControl,
        1,
        h,
        LocalControl::NoLocalControl
    );

    channel_test!(notes_off, [0x00, 0xB1, 0x7B, 0x00], NotesOff, 1);
    channel_test!(omni_off, [0x00, 0xB1, 0x7C, 0x00], OmniOff, 1);
    channel_test!(omni_on, [0x00, 0xB1, 0x7D, 0x00], OmniOn, 1);
    channel_test!(mono_on, [0x00, 0xB1, 0x7E, 0x00], MonoMode, 1);
    channel_test!(poly_on, [0x00, 0xBF, 0x7F, 0x00], PolyMode, 15);
}

fn parse_realtime_arr(msg: &[u8; 4]) -> Option<RealTime> {
    match msg[1] {
        0xF8 => Some(RealTime::Timing),
        0xF9 => Some(RealTime::Start),
        0xFA => Some(RealTime::Continue),
        0xFB => Some(RealTime::Stop),
        0xFC => Some(RealTime::KeepAlive),
        0xFD => Some(RealTime::SystemReset),
        _ => None,
    }
}

fn parse_midi(msg: &[u8; 4]) -> Option<MidiMessage> {
    parse_voice_arr(msg)
        .map(MidiMessage::MidiVoice)
        .or_else(|| parse_channel_arr(msg).map(MidiMessage::MidiChannel))
        .or_else(|| parse_realtime_arr(msg).map(MidiMessage::MidiRealTime))
}

fn parse_usb_midi(msg: &[u8; 4]) -> Option<UsbMidiMessage> {
    let cable = Cable::mask(msg[0] >> 4);
    parse_midi(msg).map(|m| UsbMidiMessage(cable, m))
}

pub struct UsbMidiItr<'a>(&'a [u8], u8);

impl<'a> Iterator for UsbMidiItr<'a> {
    type Item = UsbMidiMessage;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.1.saturating_add(4);
        let msg = self.0.get(self.1 as usize..next as usize).and_then(|sl| {
            let mut arr = [0u8; 4];
            arr.copy_from_slice(sl);
            parse_usb_midi(&arr)
        });
        self.1 = next;

        msg
    }
}

pub fn usb_midi_msgs<'a>(buffer: &'a [u8]) -> UsbMidiItr<'a> {
    UsbMidiItr(buffer, 0)
}
