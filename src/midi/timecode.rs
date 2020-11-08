use super::small_ints::*;
use core::marker::PhantomData;

pub enum MsbBits {}
pub enum LsbBits {}

pub trait Nybble {}

impl Nybble for MsbBits {}
impl Nybble for LsbBits {}

pub enum FrameBits {}
pub enum SecondsBits {}
pub enum MinutesBits {}

pub struct HoursBits;

pub trait TimePart {}

impl TimePart for FrameBits {}
impl TimePart for SecondsBits {}
impl TimePart for MinutesBits {}
impl TimePart for HoursBits {}

pub struct TimeFragment<P, B>(U4, PhantomData<P>, PhantomData<B>)
where
    P: TimePart,
    B: Nybble;

pub enum Timecode {
    Fps24 = 0x00,
    Fps25 = 0x01,
    Fps30Drop = 0x02,
    Fps30NonDrop = 0x03,
}

impl From<u8> for Timecode {
    fn from(raw: u8) -> Self {
        match raw >> 5 {
            0 => Timecode::Fps24,
            1 => Timecode::Fps25,
            2 => Timecode::Fps30Drop,
            3 => Timecode::Fps30NonDrop,
            _ => unreachable!(),
        }
    }
}

pub struct Time {
    pub frame: U5,
    pub seconds: U6,
    pub minutes: U6,
    pub hours: U5,
    pub timecode: Timecode,
}

pub struct TimeAcc {
    pub frame_low: Option<TimeFragment<FrameBits, LsbBits>>,
    pub frame_high: Option<TimeFragment<FrameBits, MsbBits>>,

    pub seconds_low: Option<TimeFragment<SecondsBits, LsbBits>>,
    pub seconds_high: Option<TimeFragment<SecondsBits, MsbBits>>,

    pub minutes_low: Option<TimeFragment<MinutesBits, LsbBits>>,
    pub minutes_high: Option<TimeFragment<MinutesBits, MsbBits>>,

    pub hours_low: Option<TimeFragment<HoursBits, LsbBits>>,
    pub hours_high: Option<TimeFragment<HoursBits, MsbBits>>,
}

impl Time {
    pub fn assemble_time(acc: TimeAcc) -> Time {
        let h8: u8 = FromU4s::from_u4s(acc.hours_low.unwrap().0, acc.hours_high.unwrap().0);
        let tc = h8.into();

        Time {
            frame: FromU4s::from_u4s(acc.frame_low.unwrap().0, acc.frame_high.unwrap().0),
            seconds: FromU4s::from_u4s(acc.seconds_low.unwrap().0, acc.seconds_high.unwrap().0),
            minutes: FromU4s::from_u4s(acc.minutes_low.unwrap().0, acc.minutes_high.unwrap().0),
            hours: U5::mask(h8),
            timecode: tc,
        }
    }
}
