use salsa::DebugWithDb;

#[derive(Eq, PartialEq, Debug, Hash, Clone, Copy, DebugWithDb)]
pub struct SourceSpan {
    pub start: usize,
    pub end: usize,
}

impl SourceSpan {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn unknown() -> Self {
        // TODO: find a better representation?
        Self { start: 0, end: 0 }
    }

    /// Alias for `unknown()`, but we can grep for it and gradually add sensible locations
    pub fn todo() -> Self {
        Self::unknown()
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub struct Located<T>(pub SourceSpan, pub T);

impl<T> Located<T> {
    pub fn new(span: SourceSpan, x: T) -> Self {
        Self(span, x)
    }

    pub fn into_inner(self) -> T {
        self.1
    }

    pub fn span(&self) -> SourceSpan {
        self.0
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

impl<T> Commented<T> {
    pub fn into_inner(self) -> T {
        self.1
    }
}

impl<T> std::ops::Deref for Commented<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.1
    }
}
