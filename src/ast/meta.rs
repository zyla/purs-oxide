use salsa::DebugWithDb;

use crate::ModuleId;

use super::AbsoluteName;

#[derive(Eq, PartialEq, Debug, Hash, Clone, Copy, DebugWithDb)]
pub struct SourceSpan {
    pub decl: SpanDeclRef,
    pub start: usize,
    pub end: usize,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Copy, DebugWithDb)]
pub enum SpanDeclRef {
    Module(ModuleId),
    Decl(AbsoluteName),
    Unknown,
}

impl SourceSpan {
    pub fn new_in_module(start: usize, end: usize, module_id: ModuleId) -> Self {
        Self {
            start,
            end,
            decl: SpanDeclRef::Module(module_id),
        }
    }

    pub fn new_relative(start: usize, end: usize, name: AbsoluteName) -> Self {
        Self {
            start,
            end,
            decl: SpanDeclRef::Decl(name),
        }
    }

    pub fn to_relative(&mut self, name: AbsoluteName, reference_loc: usize) {
        self.decl = SpanDeclRef::Decl(name);
        self.start = self.start - reference_loc;
        self.end = self.end - reference_loc;
    }

    pub fn unknown() -> Self {
        // TODO: find a better representation?
        Self {
            start: 0,
            end: 0,
            decl: SpanDeclRef::Unknown,
        }
    }

    /// Alias for `unknown()`, but we can grep for it and gradually add sensible locations
    pub fn todo() -> Self {
        Self::unknown()
    }

    pub fn to_file_location(&self, _db: &dyn crate::Db) -> (String, usize, usize) {
        todo!()
    }
}

pub trait ToSourceSpan {
    fn to_source_span(&self, module_id: ModuleId) -> SourceSpan;
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

    pub fn to_relative_span(&mut self, name: AbsoluteName, reference_loc: usize) {
        self.0.to_relative(name, reference_loc)
    }
}

impl<T> std::ops::Deref for Located<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<T> std::ops::DerefMut for Located<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.1
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
