pub const PRIM_SOURCE: &str = r"module Prim where
data Function :: Type -> Type -> Type
data Array :: Type -> Type
data Record :: Row Type -> Type
data Number :: Type
data Int :: Type
data String :: Type
data Char :: Type
data Boolean :: Type
data Type :: Type
data Constraint :: Type
data Symbol :: Type
data Row :: Type -> Type
";
