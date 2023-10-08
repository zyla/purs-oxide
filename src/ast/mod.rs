pub mod declarations;
pub mod expr;
pub mod meta;
pub mod types;

use crate::ModuleId;
pub use declarations::*;
pub use expr::*;
pub use meta::*;
pub use types::*;

use crate::symbol::Symbol;

#[salsa::interned]
pub struct AbsoluteName {
    pub module: ModuleId,
    pub name: Symbol,
}

#[salsa::interned]
pub struct QualifiedName {
    pub module: Option<ModuleId>,
    pub name: Symbol,
}

impl QualifiedName {
    pub fn is_actually_qualified(&self, db: &dyn crate::Db) -> bool {
        self.module(db).is_some()
    }

    pub fn new_unqualified(db: &dyn crate::Db, name: Symbol) -> Self {
        Self::new(db, None, name)
    }

    pub fn new_qualified(db: &dyn crate::Db, module: ModuleId, name: Symbol) -> Self {
        Self::new(db, Some(module), name)
    }
}
