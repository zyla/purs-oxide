use super::{Located, QualifiedName, Symbol};

pub type Type = Located<TypeKind>;

#[derive(Debug)]
pub enum TypeKind {
    Unknown(u64),
    Var(Symbol),
    TypeLevelString(String),
    TypeLevelInt(num::BigInt),
    Wildcard(WildcardKind),
    TypeConstructor(QualifiedName),
    TypeApp(Box<Type>, Box<Type>),
    KindApp(Box<Type>, Box<Type>),
    ForAll {
        name: Symbol,
        kind: Option<Box<Type>>,
        body: Box<Type>,
        skolem_scope: Option<SkolemScope>,
    },
    Constrained {
        constraint: Constraint,
        body: Box<Type>,
    },
    // TODO: I have no idea what these parameters mean
    Skolem(Symbol, Option<Box<Type>>, u64, SkolemScope),
    REmpty,
    RCons {
        label: Symbol,
        r#type: Box<Type>,
        rest: Box<Type>,
    },
    Kinded {
        r#type: Box<Type>,
        kind: Box<Type>,
    },
    BinaryNoParensType(Box<Type>, Box<Type>, Box<Type>),
    Parens(Box<Type>),
}

#[derive(Debug)]
pub enum WildcardKind {
    Hole(Symbol),
    Unnamed,
    Ignored,
}

#[derive(Debug)]
pub struct SkolemScope(pub u64);

#[derive(Debug)]
pub struct Constraint {
    pub class: QualifiedName,
    pub kind_args: Vec<Type>,
    pub type_args: Vec<Type>,
    // TODO: PartialConstraintData - do we need it?
}
