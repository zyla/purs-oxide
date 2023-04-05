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
pub struct QualifiedName(pub Symbol);

impl QualifiedName {
    pub fn is_actually_qualified(&self) -> bool {
        self.0 .0.contains('.')
    }
}
