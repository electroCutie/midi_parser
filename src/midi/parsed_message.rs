use super::small_ints::*;

macro_rules! wrap_small_int {
    ($name: ident ($small_int: ty) ) => {
        #[derive(PartialEq, Eq, Copy, Clone, Debug)]
        pub struct $name($small_int);

        impl From<$name> for <$small_int as WrapsInt>::Wrapped {
            fn from(x: $name) -> Self {
                x.0.into()
            }
        }

        impl From<$name> for $small_int {
            fn from(x: $name) -> Self {
                x.0
            }
        }

        impl From<$small_int> for $name {
            fn from(x: $small_int) -> Self {
                $name(x)
            }
        }

        impl $name {
            pub fn mask(wrapped: <$small_int as WrapsInt>::Wrapped) -> Self {
                $name(<$small_int>::mask(wrapped))
            }
        }
    };
}

wrap_small_int!(Cable(U4));
wrap_small_int!(Channel(U4));
wrap_small_int!(Velocity(U7));
wrap_small_int!(Note(U7));
wrap_small_int!(Program(U7));
wrap_small_int!(Pressure(U7));
wrap_small_int!(ControllerNum(U7));
wrap_small_int!(ControllerValue(U7));
wrap_small_int!(Bend(U14));

impl Note {
    pub const MIDDLE_C: Note = Note(U7::mask(60));
    pub const MIDDLE_A: Note = Note(U7::mask(58));
}

#[derive(Debug, PartialEq)]
pub enum LocalControl {
    ControlLocally,
    NoLocalControl,
}

impl From<U7> for LocalControl {
    fn from(b: U7) -> Self {
        match b.into() {
            0u8 => LocalControl::NoLocalControl,
            _ => LocalControl::ControlLocally,
        }
    }
}

pub struct UsbMidiMessage(pub Cable, pub MidiMessage);

pub enum MidiMessage {
    MidiVoice(VoiceMessage),
    MidiChannel(ChannelMessage),
    MidiRealTime(RealTime),
    MidiSystem(SystemCommon),
    // System Exlcusive intentionally omitted
}

#[derive(Debug)]
pub enum VoiceMessage {
    NoteOff(Channel, Note, Velocity),
    NoteOn(Channel, Note, Velocity),
    Pressure(Channel, Note, Pressure),
    Controller(Channel, ControllerNum, ControllerValue),
    ProgramChange(Channel, Pressure),
    ChannelPressure(Channel, Pressure),
    PitchBend(Channel, Bend),
}

#[derive(Debug)]
pub enum ChannelMessage {
    SoundOff(Channel),
    Reset(Channel),
    ChangeControl(Channel, LocalControl),
    NotesOff(Channel),
    OmniOff(Channel),
    OmniOn(Channel),
    MonoMode(Channel),
    PolyMode(Channel),
}

#[derive(Debug)]
pub enum RealTime {
    Timing,
    Start,
    Continue,
    Stop,
    KeepAlive,
    SystemReset,
}

#[derive(Debug)]
pub enum SystemCommon {
    MtcQuarterFrame(U7),
    SongPosition(U14),
    SongSelect(U7),
    Retune(),
}

#[derive(Debug)]
pub enum SystemExclusive<'a> {
    Exclusive(u16, usize, &'a [u8]),
    ExclusiveEnd,
}
