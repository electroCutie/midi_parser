enum CodeIndex {
    // 0 and 1 reserved
    SystemCommon2b = 0x02,
    SystemCommon3b = 0x03,

    SysExStart = 0x4,
    SysExEnd1b = 0x5,
    SysExEnd2b = 0x6,
    SysExEnd3b = 0x7,

    NoteOff = 0x8,
    NoteOn = 0x9,

    PolyKeypress = 0xa,
    ControlChange = 0xb,
    ProgramChange = 0xc,
    ChannelPressure = 0xd,
    PitchBend = 0xe,
    SingleByte = 0xf,
}

impl CodeIndex {
    pub fn msg_length(&self) -> u8 {
        match self {
            CodeIndex::SystemCommon2b => 2,
            CodeIndex::SystemCommon3b => 3,
            CodeIndex::SysExStart => 3,
            CodeIndex::SysExEnd1b => 1,
            CodeIndex::SysExEnd2b => 2,
            CodeIndex::SysExEnd3b => 3,
            CodeIndex::NoteOff => 3,
            CodeIndex::NoteOn => 3,
            CodeIndex::PolyKeypress => 3,
            CodeIndex::ControlChange => 3,
            CodeIndex::ProgramChange => 2,
            CodeIndex::ChannelPressure => 2,
            CodeIndex::PitchBend => 3,
            CodeIndex::SingleByte => 1,
        }
    }
}
#[derive(Debug, Copy, Clone)]
pub enum MidiByte {
    StatusByte(u8),
    DataByte(u8),
}

impl MidiByte {
    pub fn new(raw: u8) -> MidiByte {
        if raw >= 0x80 {
            MidiByte::StatusByte(raw)
        } else {
            MidiByte::DataByte(raw)
        }
    }

    fn raw(self) -> u8 {
        match self {
            MidiByte::StatusByte(b) => b,
            MidiByte::DataByte(b) => b,
        }
    }

    pub fn high_nybble(self) -> u8 {
        self.raw() >> 4
    }

    pub fn low_nybble(self) -> u8 {
        self.raw() & 0xff
    }
}
