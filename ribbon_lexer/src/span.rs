use std::fmt::Display;

use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub struct Span {
    pub low: usize,
    pub hi: usize,
}

impl Span {
    pub fn new(low: usize, hi: usize) -> Self {
        Span { low, hi }
    }

    pub fn to(&self, other: Self) -> Self {
        Span {
            low: self.low,
            hi: other.hi,
        }
    }
}

impl From<usize> for Span {
    fn from(value: usize) -> Self {
        Span::new(value, value)
    }
}

impl From<(usize, usize)> for Span {
    fn from(value: (usize, usize)) -> Self {
        Span::new(value.0, value.1)
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.low, self.hi)
    }
}

impl Default for Span {
    fn default() -> Self {
        Self { low: 0, hi: 0 }
    }
}
