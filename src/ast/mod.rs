pub mod declarations;
pub mod meta;
pub mod types;

use crate::symbol::Symbol;
pub use declarations::*;
pub use meta::*;
pub use types::*;

#[derive(Debug)]
pub struct QualifiedName(pub Symbol);
