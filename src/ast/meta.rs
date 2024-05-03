use salsa::DebugWithDb;

use crate::{renamed_module::DeclId, source_span::*};

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

impl<T> SourceSpanOps for Located<T> {
    fn to_relative_span(&mut self, decl_id: DeclId, reference_loc: usize) -> &Self {
        self.0.to_relative_span(decl_id, reference_loc);
        self
    }
    fn to_absolute_span(&mut self, module: crate::ModuleId, reference_loc: usize) -> &Self {
        self.0.to_absolute_span(module, reference_loc);
        self
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
