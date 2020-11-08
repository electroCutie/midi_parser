pub enum Disjunction<L, R> {
    Left(L),
    Right(R),
}

use Disjunction::*;

impl<X> Disjunction<X, X> {
    pub fn merge_matched(self) -> X {
        match self {
            Left(x) => x,
            Right(x) => x,
        }
    }
}

impl<L, R> Disjunction<L, R> {
    pub fn merge_into<C, Fl, Fr>(self, fl: Fl, fr: Fr) -> C
    where
        Fl: FnOnce(L) -> C,
        Fr: FnOnce(R) -> C,
    {
        match self {
            Left(l) => fl(l),
            Right(r) => fr(r),
        }
    }

    pub fn merge<C, Fl, Fr>(&self, fl: Fl, fr: Fr) -> C
    where
        Fl: FnOnce(&L) -> C,
        Fr: FnOnce(&R) -> C,
    {
        match self {
            Left(l) => fl(l),
            Right(r) => fr(r),
        }
    }
}
