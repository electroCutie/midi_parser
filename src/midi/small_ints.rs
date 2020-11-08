use core::mem;

pub trait WrapsInt {
    type Wrapped;
}

macro_rules! small_int {
    ($name: ident ($wrapped: ty), $test: ident, $bits: literal $(, { $also_impl: item } )?) => {
        #[derive(PartialEq, Eq, Copy, Clone, Debug)]
        pub struct $name($wrapped);

        impl From<$name> for $wrapped {
            fn from(u: $name) -> Self {
                u.0
            }
        }

        impl WrapsInt for $name {
            type Wrapped = $wrapped;
        }

        impl $name {
            const MASK: $wrapped = (1 << $bits) - 1;
            // This should be equivilent to MASK
            // but say it a different way to ensure that a certain error can't occur
            pub const MAX: $name = $name(<$wrapped>::MAX >> (mem::size_of::<$wrapped>() * 8 - $bits));
            pub const MIN: $name = $name(0);

            pub const fn mask(u: $wrapped) -> $name{
                $name(u & <$name>::MASK)
            }

            $( $also_impl )?
        }

        #[cfg(test)]
        mod $test {
            use super::*;
            #[test]
            fn test_mask() {
                let min_expected = <$name>::MIN.0;
                let max_expected = <$name>::MAX.0;
                let mut x = <$wrapped>::MAX;
                loop {
                    let w = $name::mask(x).0;
                    assert!(w <= max_expected, "{} > {}", w, max_expected);
                    assert!(w >= min_expected, "{} < {}", w, min_expected);

                    if x == 0 {
                        break
                    }
                    x = x >> 1;
                }
            }
        }
    };

}

small_int!(U4(u8), u4_test, 4);
small_int!(U5(u8), u5_test, 5);
small_int!(U6(u8), u6_test, 6);
small_int!(U7(u8), u7_test, 7);
small_int!(U14(u16), u14_test, 14, {
    pub fn from_u7s(msb: U7, lsb: U7) -> U14 {
        let msb16: u16 = msb.0 as u16;
        let lsb16: u16 = lsb.0 as u16;
        U14::mask((msb16 << 7) | lsb16)
    }
});

pub trait FromU4s {
    fn from_u4s(lsb: U4, msb: U4) -> Self;
}

macro_rules! from_u4s_impl {
    ($name: ty) => {
        impl FromU4s for $name {
            fn from_u4s(lsb: U4, msb: U4) -> Self {
                <$name>::mask((msb.0 << 4) | lsb.0)
            }
        }
    };
}

from_u4s_impl!(U5);
from_u4s_impl!(U6);
from_u4s_impl!(U7);

impl FromU4s for u8 {
    fn from_u4s(lsb: U4, msb: U4) -> Self {
        (msb.0 << 4) | lsb.0
    }
}
