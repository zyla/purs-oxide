use std::fmt::Display;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct LexerError(pub String);

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct Loc {
    pub start: usize,
    pub end: usize,
}

impl Loc {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Error {
    pub loc: Loc,
    pub kind: ErrorKind,
}

impl Error {
    pub fn new(start: usize, end: usize, kind: ErrorKind) -> Self {
        Self {
            loc: Loc::new(start, end),
            kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    InvalidClassHead,
    InvalidInstanceHead,
    InvalidFloatingPointNumber,
    NonUsvChar,
    Unknown(String),
    Error(LexerError),
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
