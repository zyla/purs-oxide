#[derive(Debug)]
pub struct SourceSpan {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug)]
pub struct Located<T>(pub SourceSpan, pub T);

#[derive(Debug)]
pub struct Comment(pub String);

#[derive(Debug)]
pub struct Commented<T>(pub Vec<Comment>, pub T);
