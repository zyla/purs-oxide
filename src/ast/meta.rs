#[derive(Debug)]
pub struct SourceSpan {
    pub start: usize,
    pub end: usize,
}

impl SourceSpan {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Debug)]
pub struct Located<T>(pub SourceSpan, pub T);

impl<T> Located<T> {
    pub fn into_inner(self) -> T {
        self.1
    }
}

#[derive(Debug)]
pub struct Comment(pub String);

#[derive(Debug)]
pub struct Commented<T>(pub Vec<Comment>, pub T);
