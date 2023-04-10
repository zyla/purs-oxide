use salsa::DebugWithDb;

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub struct SourceSpan {
    pub start: usize,
    pub end: usize,
}

impl SourceSpan {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub struct Located<T>(pub SourceSpan, pub T);

impl<T> Located<T> {
    pub fn into_inner(self) -> T {
        self.1
    }
}

impl<T> std::ops::Deref for Located<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub struct Comment(pub String);

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub struct Commented<T>(pub Vec<Comment>, pub T);

impl<T> std::ops::Deref for Commented<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.1
    }
}
