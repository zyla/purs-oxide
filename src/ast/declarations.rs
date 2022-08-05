use super::{Commented, Located, Type};
use crate::symbol::Symbol;

pub type Module = Located<Commented<ModuleInner>>;

#[derive(Debug)]
pub struct ModuleInner {
    pub name: Symbol,
    pub exports: Option<Vec<DeclarationRef>>,
    pub declarations: Vec<Declaration>,
}

pub type DeclarationRef = Located<DeclarationRefKind>;

#[derive(Debug)]
pub enum NameSource {
    UserNamed,
    CompilerNamed,
}

pub type ModuleName = Symbol;

#[derive(Debug)]
pub enum DeclarationRefKind {
    TypeClass {
        name: Symbol,
    },
    TypeOp {
        name: Symbol,
    },
    Type {
        name: Symbol,
        constructors: Option<Vec<Symbol>>,
    },
    Value {
        name: Symbol,
    },
    ValueOp {
        name: Symbol,
    },
    TypeInstanceRef {
        name: Symbol,
        name_source: NameSource,
    },
    Module {
        name: ModuleName,
    },
    ReExport {
        imported_from: ModuleName,
        defined_in: ModuleName,
        declaration_ref: Box<DeclarationRef>,
    },
}

#[derive(Debug)]
pub enum ImportDeclarationKind {
    Implicit,
    Explicit(Vec<DeclarationRef>),
    Hiding(Vec<DeclarationRef>),
}

#[derive(Debug)]
pub struct RoleDeclarationData {
    pub ident: Symbol,
    pub role: Vec<Role>,
}

#[derive(Debug)]
pub enum Role {
    Nominal,
    Representational,
    Phantom,
}

#[derive(Debug)]
pub struct TypeDeclarationData {
    pub ident: Symbol,
    pub r#type: Type,
}

pub type Declaration = Located<Commented<DeclarationKind>>;

#[derive(Debug)]
pub enum DeclarationKind {
    Data {
        r#type: DataDeclType,
        name: Symbol,
        params: Vec<(Symbol, Option<Type>)>,
        constructors: Vec<DataConstructorDeclaration>,
    },
    ValueDeclaration(ValueDeclaration),
}

pub type DataConstructorDeclaration = Located<Commented<DataConstructorDeclarationData>>;

#[derive(Debug)]
pub struct DataConstructorDeclarationData {
    pub name: Symbol,

    // TODO: why do they have names? I thought datacon fields are unnamed in PS
    pub fields: Vec<(Symbol, Type)>,
}

#[derive(Debug)]
pub enum DataDeclType {
    Data,
    Newtype,
}

#[derive(Debug)]
pub struct ValueDeclaration {
    pub ident: Symbol,

    // TODO: what is this?
    // pub name: NameKind

    // TODO
    // pub binders: Vec<Binder>
    pub expr: Vec<GuardedExpr>,
}

#[derive(Debug)]
pub struct GuardedExpr {
    pub guards: Vec<Guard>,
    pub expr: Expr,
}

#[derive(Debug)]
pub enum Guard {
    // TODO
}

pub type Expr = Located<ExprKind>;

#[derive(Debug)]
pub enum ExprKind {
    Literal(Literal<Expr>),
    // TODO
}

#[derive(Debug)]
pub enum Literal<T> {
    Integer(i32),
    Float(f64),
    String(String),
    Char(char),
    Boolean(bool),
    Array(Vec<T>),
    Object(Vec<(Symbol, T)>),
}
