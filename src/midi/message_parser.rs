use super::eager_parser::*;
use super::parsed_message::*;
use super::small_ints::*;

fn voice_3<'a, N, V, F, Msg>(
    prefix: u8,
    f: F,
) -> impl Fn(&Parser<'a, (u8, Channel)>) -> Parser<'a, Msg>
where
    N: From<U7>,
    V: From<U7>,
    F: FnOnce(Channel, N, V) -> Msg + Copy,
{
    move |p| {
        p.filter(|(t, _)| *t == prefix)
            .map_into(|t| t.1)
            .and(|p| p.aligned_byte().map_into(U7::mask).into_type())
            .and(|p| p.aligned_byte().map_into(U7::mask).into_type())
            .map_into_3(f)
    }
}

fn voice_2<'a, N, F, Msg>(
    prefix: u8,
    f: F,
) -> impl Fn(&Parser<'a, (u8, Channel)>) -> Parser<'a, Msg>
where
    N: From<U7>,
    F: FnOnce(Channel, N) -> Msg + Copy,
{
    move |p| {
        p.filter(|(t, _)| *t == prefix)
            .map_into(|t| t.1)
            .and(|p| p.aligned_byte().map_into(U7::mask).into_type())
            .map_into_2(f)
    }
}

fn parse_voice<'a>(parser: Parser<'a, ()>) -> Parser<'a, VoiceMessage> {
    parser
        .nybble()
        .into_type::<u8>()
        // All voice messages are 8n,9n,an,bn,cn,dn,en (not f)
        .filter(|n| *n >= 0x8 && *n < 0xf)
        // Channel
        .and(|p| p.nybble().into_type::<Channel>())
        .branch()
        //Note On
        .or_with(voice_3(0x09, |c, n, v: Velocity| {
            match v.into() {
                0u8 => VoiceMessage::NoteOff(c, n, Velocity::mask(0x40)), // Note on with 0 is really an off
                _ => VoiceMessage::NoteOn(c, n, v),
            }
        }))
        .or_with(voice_3(0x08, VoiceMessage::NoteOff))
        .or_with(voice_3(0x0a, VoiceMessage::Pressure))
        .or_with(|p| {
            let f = voice_3(0x0b, VoiceMessage::Controller);
            // 0x79-0x7f are actually Channel mode messages
            let p = p.peek(|l| l.aligned_byte().filter(|b| *b < 0x78 || *b >= 0x80));
            f(&p)
        })
        .or_with(voice_2(0x0c, VoiceMessage::ProgramChange))
        .or_with(voice_2(0x0d, VoiceMessage::ChannelPressure))
        .or_with(voice_3(0x0e, |c, lsb, msb| {
            VoiceMessage::PitchBend(c, U14::from_u7s(msb, lsb).into())
        }))
        .or_nothing()
}

#[cfg(test)]
mod test_voice {
    use super::*;

    macro_rules! voice_test {
        ($name: ident, $bytes: tt, $cons: ident, $chan: literal, $first: literal $(, $heck: ident, $second: literal)? ) => {
            #[test]
            fn $name (){
                let bytes = $bytes;
                let p = Parser::new(&bytes);
                let v = parse_voice(p).to_option().unwrap();
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

    #[rustfmt::skip] voice_test!(note_on, [0x91, 0x3c, 0x44], NoteOn, 1, 0x3Cu8, h, 0x44u8);
    #[rustfmt::skip] voice_test!(note_on_0_velocity, [0x91, 0x3c, 0x00], NoteOff, 1, 0x3Cu8, h, 0x40u8);
    #[rustfmt::skip] voice_test!(note_off, [0x82, 0x3c, 0x30], NoteOff, 2, 0x3Cu8, h, 0x30u8);
    #[rustfmt::skip] voice_test!(poly_pressure, [0xA2, 0x3c, 0x35], Pressure, 2, 0x3Cu8, h, 0x35u8);
    #[rustfmt::skip] voice_test!(controller, [0xB2, 0x77, 0x35], Controller, 2, 0x77u8, h, 0x35u8);
    #[rustfmt::skip] voice_test!(pressure, [0xB2, 0x77, 0x35], Controller, 2, 0x77u8, h, 0x35u8);

    #[rustfmt::skip] voice_test!(program, [0xC2, 0x14], ProgramChange, 2, 0x14u8);
    #[rustfmt::skip] voice_test!(channel_pressure, [0xD2, 0x14], ChannelPressure, 2, 0x14u8);

    #[rustfmt::skip] voice_test!(bend, [0xE2, 0b0101010, 0b01111111], PitchBend, 2, 0b11111110101010u16);

    #[test]
    fn controller_as_channel() {
        let bytes = [0xB2, 0x78, 0x35];
        let p = Parser::new(&bytes);
        assert!(parse_voice(p).to_option().is_none());
    }
}

fn channel_no_data<'a, F, Msg>(prefix: u8, f: F) -> impl Fn(&Parser<'a, Channel>) -> Parser<'a, Msg>
where
    F: FnOnce(Channel) -> Msg + Copy,
{
    move |p| {
        p.discarding(
            |p| p.aligned_byte().match_value(prefix).aligned_byte(), /* ends in 0x00 padding */
        )
        .map_into(f)
    }
}

fn parse_channel<'a>(parser: Parser<'a, ()>) -> Parser<'a, ChannelMessage> {
    parser
        .nybble()
        .into_type()
        .match_into(0xb)
        .nybble()
        .map_into(Channel::from)
        .branch()
        .or_with(channel_no_data(0x78, ChannelMessage::SoundOff))
        .or_with(channel_no_data(0x79, ChannelMessage::Reset))
        .or_with(|p| {
            p.discarding(|p| p.aligned_byte().match_value(0x7a))
                .and(|p| p.aligned_byte())
                .map_into(|(ch, lo)| {
                    ChannelMessage::ChangeControl(
                        ch,
                        match lo {
                            0 => LocalControl::NoLocalControl,
                            _ => LocalControl::ControlLocally,
                        },
                    )
                })
        })
        .or_with(channel_no_data(0x7b, ChannelMessage::NotesOff))
        .or_with(channel_no_data(0x7c, ChannelMessage::OmniOff))
        .or_with(channel_no_data(0x7d, ChannelMessage::OmniOn))
        .or_with(channel_no_data(0x7e, ChannelMessage::MonoMode))
        .or_with(channel_no_data(0x7f, ChannelMessage::PolyMode))
        .or_nothing()
}

#[cfg(test)]
mod test_channel {
    use super::super::parsed_message::LocalControl;
    use super::*;

    macro_rules! channel_test {
        ($name: ident, $bytes: tt, $cons: ident, $chan: literal $(, $heck: ident, $first: path)? ) => {
            #[test]
            fn $name (){
                let bytes = $bytes;
                let p = Parser::new(&bytes);
                let v = parse_channel(p).to_option().unwrap();
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

    channel_test!(sound_off, [0xB1, 0x78, 0x00], SoundOff, 1);
    channel_test!(reset, [0xB1, 0x79, 0x00], Reset, 1);

    channel_test!(
        local_control_on,
        [0xB1, 0x7A, 0x7F],
        ChangeControl,
        1,
        h,
        LocalControl::ControlLocally
    );
    channel_test!(
        local_control_off,
        [0xB1, 0x7A, 0x00],
        ChangeControl,
        1,
        h,
        LocalControl::NoLocalControl
    );

    channel_test!(notes_off, [0xB1, 0x7B, 0x00], NotesOff, 1);
    channel_test!(omni_off, [0xB1, 0x7C, 0x00], OmniOff, 1);
    channel_test!(omni_on, [0xB1, 0x7D, 0x00], OmniOn, 1);
    channel_test!(mono_on, [0xB1, 0x7E, 0x00], MonoMode, 1);
    channel_test!(poly_on, [0xBF, 0x7F, 0x00], PolyMode, 15);
}

fn parse_realtime<'a>(parser: Parser<'a, ()>) -> Parser<'a, RealTime> {
    parser
        .aligned_byte()
        .branch()
        .or_with(|p| p.match_value(0xF8).pure(RealTime::Timing))
        .or_with(|p| p.match_value(0xFA).pure(RealTime::Start))
        .or_with(|p| p.match_value(0xFB).pure(RealTime::Continue))
        .or_with(|p| p.match_value(0xFC).pure(RealTime::Stop))
        .or_with(|p| p.match_value(0xFE).pure(RealTime::KeepAlive))
        .or_with(|p| p.match_value(0xFF).pure(RealTime::SystemReset))
        .or_nothing()
}

fn midi_parser<'a>(parser: Parser<'a, ()>) -> Parser<'a, MidiMessage> {
    parser
        .branch()
        .or(|p| parse_voice(p).map_into(MidiMessage::MidiVoice))
        .or(|p| parse_channel(p).map_into(MidiMessage::MidiChannel))
        .or(|p| parse_realtime(p).map_into(MidiMessage::MidiRealTime))
        .or_nothing()
}

pub fn parse_midi<'a>(parser: &Parser<'a, ()>) -> Option<MidiMessage> {
    midi_parser(parser.clone()).to_option()
}

pub struct UsbMidiItr<'a>(Parser<'a, ()>);

fn usb_midi_parser<'a>(parser: &Parser<'a, ()>) -> Parser<'a, UsbMidiMessage> {
    parser
        .nybble()
        .into_type()
        // Discard the USB msg type, we parse the midi message
        .discarding(Parser::nybble)
        .and(midi_parser)
        .map_into_2(UsbMidiMessage)
}

impl<'a> Iterator for UsbMidiItr<'a> {
    type Item = UsbMidiMessage;

    fn next(&mut self) -> Option<Self::Item> {
        let msg = usb_midi_parser(&self.0);
        self.0 = msg.pure(());
        msg.to_option()
    }
}

pub fn parse_usb_midi<'a>(parser: Parser<'a, ()>) -> UsbMidiItr<'a> {
    UsbMidiItr(parser)
}
