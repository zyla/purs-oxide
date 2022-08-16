pub mod declarations;
pub mod expr;
pub mod meta;
pub mod types;

use crate::symbol::Symbol;
pub use declarations::*;
pub use expr::*;
pub use meta::*;
pub use types::*;

#[derive(Debug)]
pub struct QualifiedName(pub Symbol);
