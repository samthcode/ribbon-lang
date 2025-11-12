#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    low: u32,
    hi: u32,
}

impl Span {
    fn new(low: u32, hi: u32) -> Self {
        Span { low, hi }
    }

    fn grow_to(&mut self, pos: u32) {
        assert!(
            self.low >= pos && self.hi <= pos,
            "pos must not lie within span"
        );
        if self.low > pos {
            self.low = pos
        } else {
            self.hi = pos;
        }
    }

    fn grow_to_span(&mut self, other: Self) {
        if other.hi > self.hi {
            self.hi = other.hi
        }
        if other.low < self.low {
            self.low = other.low
        }
    }
}

impl From<u32> for Span {
    fn from(value: u32) -> Self {
        Span::new(value, value)
    }
}

impl From<(u32, u32)> for Span {
    fn from(value: (u32, u32)) -> Self {
        Span::new(value.0, value.1)
    }
}
