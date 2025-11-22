#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub low: u32,
    pub hi: u32,
}

impl Span {
    fn new(low: u32, hi: u32) -> Self {
        Span { low, hi }
    }

    pub fn to(&self, other: &Self) -> Self {
        Span {
            low: self.low,
            hi: other.hi,
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
