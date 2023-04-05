pub mod declarations;
pub mod expr;
pub mod meta;
pub mod types;

use crate::symbol::Symbol;
pub use declarations::*;
pub use expr::*;
pub use meta::*;
pub use types::*;

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct QualifiedName {
    pub symbol: Symbol,
    pub actually_qualified: bool,
}

impl QualifiedName {
    pub fn is_actually_qualified(&self) -> bool {
        self.actually_qualified
    }

    pub fn new(db: &dyn crate::Db, symbol: Symbol) -> Self {
        Self {
            symbol,
            actually_qualified: symbol.text(db).contains('.'),
        }
    }
}
