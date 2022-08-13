#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Symbol(pub String);

impl Symbol {
    pub fn new(s: String) -> Self {
        Self(s)
    }
}
