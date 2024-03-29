use super::{Located, QualifiedName, Symbol};
use crate::string::PSString;
use salsa::DebugWithDb;

pub type Type = Located<TypeKind>;

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub enum TypeKind {
    /// Only appears during typechecking
    Unknown(u64),
    Var(Symbol),
    /// Standalone operator
    Operator(QualifiedName),
    TypeLevelString(PSString),
    TypeLevelInt(num::BigInt),
    Wildcard(WildcardKind),
    TypeConstructor(QualifiedName),
    TypeApp(Box<Type>, Box<Type>),
    FunctionType(Box<Type>, Box<Type>),

    /// Kind parameter application. Doesn't seem to appear in source, because kind params are
    /// implicit?
    KindApp(Box<Type>, Box<Type>),

    ForAll {
        vars: Vec<(Symbol, Option<Box<Type>>)>,
        body: Box<Type>,

        // TODO: figure out whether this needs to be a part of vars
        skolem_scope: Option<SkolemScope>,
    },

    Constrained {
        constraint: Constraint,
        body: Box<Type>,
    },

    // TODO: I have no idea what these parameters mean
    Skolem(Symbol, Option<Box<Type>>, u64, SkolemScope),
    Row {
        fields: Vec<(Symbol, Type)>,
        rest: Option<Box<Type>>,
    },
    Kinded {
        r#type: Box<Type>,
        kind: Box<Type>,
    },
    /// Infix operator sequence with unknown precedence
    Infix(Box<Type>, Vec<(QualifiedName, Type)>),
    // We used to have this:
    // Parens(Box<Type>),
    // taken from original PureScript compiler. But it doesn't seem necessary for us, and makes it
    // harder to properly test the pretty-printer.

    // Invalid type (but we still proceed around it)
    Error,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub enum WildcardKind {
    Hole(Symbol),
    Unnamed,
    Ignored,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub struct SkolemScope(pub u64);

pub type Constraint = Box<Type>;
/*
 * for now just Type
#[derive(Debug)]
pub struct Constraint {
    pub class: QualifiedName,
    pub kind_args: Vec<Type>,
    pub type_args: Vec<Type>,
    // TODO: PartialConstraintData - do we need it?
}
*/
