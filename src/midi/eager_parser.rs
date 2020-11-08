use super::small_ints::*;

use super::disjunction::*;

pub enum Branch<'a, V, T> {
    Branching(Parser<'a, V>),
    Locked(Parser<'a, T>),
}

#[derive(Clone)]
pub enum Parser<'a, V> {
    Nothing(State<'a>),
    Parsing(State<'a>, V),
}

use Parser::*;

#[derive(Clone)]
pub struct State<'a> {
    /// Current index into the packet in Nybles
    pub ptr_nyble: u8,
    pub packet: &'a [u8],
}

const fn empty_state<'a>(bytes: &'a [u8]) -> State<'a> {
    State {
        ptr_nyble: 0,
        packet: bytes,
    }
}

impl<'a> State<'a> {
    pub fn nyble(&self) -> Parser<'a, U4> {
        let b_idx = (self.ptr_nyble >> 1) as usize;

        if b_idx >= self.packet.len() {
            return Nothing(self.clone());
        }

        let b = self.packet[b_idx];
        let b = match (!self.ptr_nyble) & 1 {
            1 => b >> 4,
            _ => b,
        };
        Parsing(
            State {
                ptr_nyble: self.ptr_nyble + 1,
                packet: self.packet,
            },
            U4::mask(b),
        )
    }

    pub fn aligned_byte(&self) -> Parser<'a, u8> {
        let b_idx = (self.ptr_nyble >> 1) as usize;

        if b_idx >= self.packet.len() || self.ptr_nyble & 1 != 0 {
            return Nothing(self.clone());
        }

        Parsing(
            State {
                ptr_nyble: self.ptr_nyble + 2,
                packet: self.packet,
            },
            self.packet[b_idx],
        )
    }
}

impl<'a> Parser<'a, ()> {
    pub fn new(bytes: &'a [u8]) -> Parser<'a, ()> {
        Parsing(empty_state(bytes), ())
    }
}

impl<'a, V> Parser<'a, V> {
    fn into_parsing<F, R>(self, f: F) -> Parser<'a, R>
    where
        F: FnOnce(State<'a>, V) -> Parser<'a, R>,
    {
        match self {
            Nothing(s) => Nothing(s),
            Parsing(state, v) => f(state, v),
        }
    }

    fn parsing<F, R>(&self, f: F) -> Parser<'a, R>
    where
        F: FnOnce(&State<'a>, &V) -> Parser<'a, R>,
    {
        match self {
            Nothing(s) => Nothing(s.clone()),
            Parsing(state, v) => f(state, v),
        }
    }

    fn get_state(&self) -> State<'a> {
        (match self {
            Nothing(s) => s,
            Parsing(s, _) => s,
        })
        .clone()
    }

    pub fn pure<T>(&self, v: T) -> Parser<'a, T> {
        self.parsing(|state, _| Parsing(state.clone(), v))
    }

    pub fn aligned_byte(&self) -> Parser<'a, u8> {
        self.parsing(|state, _| state.aligned_byte())
    }

    pub fn nybble(&self) -> Parser<'a, U4> {
        self.parsing(|state, _| state.nyble())
    }

    pub fn lookahead<P, L>(&self, p: P) -> Parser<'a, L>
    where
        P: FnOnce(&Parser<'a, V>) -> Parser<'a, L>,
    {
        self.parsing(|_, _| p(self).into_parsing(|_, l| self.pure(l)))
    }

    pub fn lookahead_into<P, L>(self, p: P) -> Parser<'a, L>
    where
        P: FnOnce(Parser<'a, V>) -> Parser<'a, L>,
    {
        self.into_parsing(|state, v| {
            p(Parsing(state.clone(), v)).into_parsing(|_, l| Parsing(state, l))
        })
    }

    pub fn peek<P, _X>(&self, p: P) -> Parser<'a, V>
    where
        P: FnOnce(&Parser<'a, V>) -> Parser<'a, _X>,
        V: Clone,
    {
        self.parsing(|state, v| p(self).parsing(|_, _| Parsing(state.clone(), v.clone())))
    }

    pub fn discarding<P, L>(&self, p: P) -> Parser<'a, V>
    where
        V: Clone,
        P: FnOnce(&Parser<'a, V>) -> Parser<'a, L>,
    {
        p(self).into_parsing(|state, _| self.parsing(|_, v| Parsing(state, v.clone())))
    }

    pub fn discarding_into<P, L>(self, p: P) -> Parser<'a, V>
    where
        V: Clone,
        P: FnOnce(Parser<'a, ()>) -> Parser<'a, L>,
    {
        p(self.pure(())).into_parsing(|state, _| self.into_parsing(|_, v| Parsing(state, v)))
    }

    pub fn filter<T>(&self, test: T) -> Parser<'a, V>
    where
        V: Clone,
        T: FnOnce(&V) -> bool,
    {
        let b = match self {
            Parsing(_, v) => test(v),
            Nothing(_) => false,
        };

        match b {
            true => self.clone(),
            false => Nothing(self.get_state()),
        }
    }

    pub fn filter_into<T>(self, test: T) -> Parser<'a, V>
    where
        T: FnOnce(&V) -> bool,
    {
        self.into_parsing(|state, v| {
            if test(&v) {
                return Parsing(state, v);
            } else {
                return Nothing(state);
            }
        })
    }

    pub fn map<M, R>(&self, mapper: M) -> Parser<'a, R>
    where
        M: FnOnce(&V) -> R,
    {
        self.parsing(|state, v| Parsing(state.clone(), mapper(v)))
    }

    pub fn map_into<M, R>(self, mapper: M) -> Parser<'a, R>
    where
        M: FnOnce(V) -> R,
    {
        self.into_parsing(|state, v| Parsing(state.clone(), mapper(v)))
    }

    pub fn or<P, Q, X, Y>(&self, p: P, q: Q) -> Parser<Disjunction<X, Y>>
    where
        P: FnOnce(Parser<'a, ()>) -> Parser<'a, X>,
        Q: FnOnce(Parser<'a, ()>) -> Parser<'a, Y>,
    {
        self.parsing(|_, _| match p(self.pure(())) {
            Nothing(_) => q(self.pure(())).map_into(Disjunction::Right),
            x => x.map_into(Disjunction::Left),
        })
    }

    pub fn or_with<P, Q, X, Y>(&self, p: P, q: Q) -> Parser<Disjunction<X, Y>>
    where
        P: FnOnce(&Parser<'a, V>) -> Parser<'a, X>,
        Q: FnOnce(&Parser<'a, V>) -> Parser<'a, Y>,
    {
        self.parsing(|_, _| match p(&self) {
            Nothing(_) => q(&self).map_into(Disjunction::Right),
            x => x.map_into(Disjunction::Left),
        })
    }

    pub fn and<P, X>(self, p: P) -> Parser<'a, (V, X)>
    where
        P: FnOnce(Parser<'a, ()>) -> Parser<'a, X>,
    {
        self.into_parsing(|state, v| p(Parsing(state, ())).map_into(|x| (v, x)))
    }

    pub fn and_with<P, X>(self, p: P) -> Parser<'a, (V, X)>
    where
        P: FnOnce(Parser<'a, &V>) -> Parser<'a, X>,
    {
        self.into_parsing(|state, v| p(Parsing(state, &v)).map_into(|x| (v, x)))
    }

    pub fn into_type<T>(self) -> Parser<'a, T>
    where
        T: From<V>,
    {
        self.map_into(T::from)
    }

    pub fn branch<T>(self) -> Branch<'a, V, T> {
        Branch::Branching(self)
    }

    pub fn to_option(self) -> Option<V> {
        match self {
            Nothing(_) => None,
            Parsing(_, v) => Some(v),
        }
    }
}

impl<'a, A, B> Parser<'a, (A, B)> {
    pub fn map_into_2<F, T>(self, f: F) -> Parser<'a, T>
    where
        F: FnOnce(A, B) -> T,
    {
        self.map_into(|(a, b)| f(a, b))
    }
}

impl<'a, A, B, C> Parser<'a, ((A, B), C)> {
    pub fn map_into_3<F, T>(self, f: F) -> Parser<'a, T>
    where
        F: FnOnce(A, B, C) -> T,
    {
        self.map_into(|((a, b), c)| f(a, b, c))
    }
}

impl<'a, A, B, C, D> Parser<'a, (((A, B), C), D)> {
    pub fn map_into_4<F, T>(self, f: F) -> Parser<'a, T>
    where
        F: FnOnce(A, B, C, D) -> T,
    {
        self.map_into(|(((a, b), c), d)| f(a, b, c, d))
    }
}

impl<'a, A, B, C, D, E> Parser<'a, ((((A, B), C), D), E)> {
    pub fn map_into_5<F, T>(self, f: F) -> Parser<'a, T>
    where
        F: FnOnce(A, B, C, D, E) -> T,
    {
        self.map_into(|((((a, b), c), d), e)| f(a, b, c, d, e))
    }
}

impl<'a, V> Parser<'a, V>
where
    V: Eq,
{
    pub fn match_into(self, to_match: V) -> Parser<'a, V> {
        self.filter_into(move |v| *v == to_match)
    }

    pub fn match_value(&self, to_match: V) -> Parser<'a, V>
    where
        V: Clone,
    {
        self.filter(move |v| *v == to_match)
    }
}

impl<'a, V, T> Branch<'a, V, T> {
    pub fn or<P>(self, parse: P) -> Branch<'a, V, T>
    where
        P: FnOnce(Parser<'a, ()>) -> Parser<'a, T>,
    {
        match &self {
            Branch::Locked(_) => self,
            Branch::Branching(p) => match parse(p.pure(())) {
                Nothing(_) => self,
                p => Branch::Locked(p),
            },
        }
    }

    pub fn or_with<P>(self, parse: P) -> Branch<'a, V, T>
    where
        P: FnOnce(&Parser<'a, V>) -> Parser<'a, T>,
    {
        match &self {
            Branch::Locked(_) => self,
            Branch::Branching(p) => match parse(&p) {
                Nothing(_) => self,
                p => Branch::Locked(p),
            },
        }
    }

    pub fn or_else(self, default: T) -> Parser<'a, T> {
        match self {
            Branch::Branching(p) => Parsing(p.get_state(), default),
            Branch::Locked(p) => p,
        }
    }

    pub fn or_nothing(self) -> Parser<'a, T> {
        match self {
            Branch::Branching(p) => Nothing(p.get_state()),
            Branch::Locked(p) => p,
        }
    }

    pub fn disjoint(self) -> Parser<'a, Disjunction<V, T>> {
        match self {
            Branch::Branching(p) => p.map_into(Disjunction::Left),
            Branch::Locked(p) => p.map_into(Disjunction::Right),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct W(u8);

    #[test]
    fn test_ref() {
        let bytes = [0u8; 4];
        let p = Parsing(empty_state(&bytes), ());

        let wrapped = W(0);
        p.pure(&wrapped).filter(|x| x.0 == 0);
    }

    #[test]
    fn test_basic_getters() {
        let mut bytes = [1u8; 2];

        // Just get a byte
        assert_eq!(1u8, Parser::new(&bytes).aligned_byte().to_option().unwrap());

        // Fail to get an unaligned byte
        assert!(Parser::new(&bytes)
            .nybble()
            .aligned_byte()
            .to_option()
            .is_none());

        // get a byte after two nybbles
        assert_eq!(
            1,
            Parser::new(&bytes)
                .nybble()
                .nybble()
                .aligned_byte()
                .to_option()
                .unwrap()
        );

        // get the very last byte
        assert_eq!(
            1u8,
            Parser::new(&bytes)
                .aligned_byte()
                .aligned_byte()
                .to_option()
                .unwrap()
        );

        // get the first nybble
        assert_eq!(
            0u8,
            Parser::new(&bytes).nybble().to_option().unwrap().into()
        );

        // get the second nybble
        assert_eq!(
            1u8,
            Parser::new(&bytes)
                .nybble()
                .nybble()
                .to_option()
                .unwrap()
                .into()
        );

        bytes[1] = 0xf0;
        // get the third nybble we jsut wrote
        assert_eq!(
            0x0fu8,
            Parser::new(&bytes)
                .aligned_byte()
                .nybble()
                .to_option()
                .unwrap()
                .into()
        );
    }

    #[test]
    fn test_higher_order_functions() {
        let bytes = [1u8; 2];

        assert_eq!(
            2,
            Parser::new(&bytes)
                .aligned_byte()
                .map_into(|x| x + 1)
                .to_option()
                .unwrap()
        );

        assert_eq!(
            2,
            Parser::new(&bytes)
                .aligned_byte()
                .map(|x| x + 1)
                .to_option()
                .unwrap()
        );

        assert!(Parser::new(&bytes)
            .aligned_byte()
            .filter(|x| *x == 2)
            .to_option()
            .is_none());

        assert_eq!(
            1,
            Parser::new(&bytes)
                .aligned_byte()
                .filter(|x| *x == 1)
                .to_option()
                .unwrap()
        );
    }

    #[test]
    fn test_or() {
        let bytes = [0, 1];

        // Second arm is reached
        assert_eq!(
            4u8,
            Parser::new(&bytes)
                .aligned_byte()
                .or_with(
                    |p| p.filter(|x| *x == 255),
                    |p| p.aligned_byte().map_into(|x| x + 3)
                )
                .map_into(Disjunction::merge_matched)
                .to_option()
                .unwrap()
        );

        // Second arm is not reached
        assert_eq!(
            0u8,
            Parser::new(&bytes)
                .aligned_byte()
                .or_with(
                    |p| p.filter(|x| *x == 0),
                    |p| p.aligned_byte().map_into(|x| x + 200)
                )
                .map_into(Disjunction::merge_matched)
                .to_option()
                .unwrap()
        );

        //First arm
        assert_eq!(
            1u8,
            Parser::new(&bytes)
                .aligned_byte() // discarded
                .or(
                    |p| p.aligned_byte().filter(|x| *x == 1),
                    |p| p.aligned_byte().map_into(|x| x + 200)
                )
                .map_into(Disjunction::merge_matched)
                .to_option()
                .unwrap()
        );

        //Second arm
        assert_eq!(
            4u8,
            Parser::new(&bytes)
                .aligned_byte() // discarded
                .or(
                    |p| p.aligned_byte().filter(|x| *x == 255),
                    |p| p.aligned_byte().map_into(|x| x + 3)
                )
                .map_into(Disjunction::merge_matched)
                .to_option()
                .unwrap()
        );
    }

    #[test]
    fn test_and() {
        let bytes = [0, 1];
        assert_eq!(
            (0u8, 1u8),
            Parser::new(&bytes)
                .aligned_byte()
                .and(|p| p.aligned_byte())
                .to_option()
                .unwrap()
        );

        assert_eq!(
            (0u8, 2u8),
            Parser::new(&bytes)
                .aligned_byte()
                .and_with(|p| p.map(|x| *x + 2))
                .to_option()
                .unwrap()
        );
    }

    #[test]
    fn test_branch() {
        let bytes = [1, 2];
        assert_eq!(
            3u8,
            Parser::new(&bytes)
                .branch()
                .or_with(|p| p.aligned_byte().filter(|x| *x > 3))
                .or_with(|p| p.nybble().map_into(U4::into).filter(|x| *x > 3))
                .or_with(|p| p
                    .nybble()
                    .nybble()
                    .map_into(U4::into)
                    .and_with(|p| p.aligned_byte())
                    .map(|(x, y)| x + y))
                .or_nothing()
                .to_option()
                .unwrap()
        );

        assert!(Parser::new(&bytes)
            .branch()
            .or_with(|p| p.aligned_byte().aligned_byte().aligned_byte())
            .or_nothing()
            .to_option()
            .is_none());
    }

    //TODO: test lookaheads, peeks, and discarding
}
