use super::{Commented, Expr, Located, Pat, Type};
use crate::ast::QualifiedName;
use crate::symbol::Symbol;

pub type Module = Located<Commented<ModuleInner>>;

#[derive(Debug)]
pub struct ModuleInner {
    pub name: ModuleName,
    pub exports: Option<Vec<DeclarationRef>>,
    pub imports: Vec<Import>,
    pub declarations: Vec<Declaration>,
}

pub type Import = Located<ImportInner>;

#[derive(Debug)]
pub struct ImportInner {
    pub module: ModuleName,
    pub kind: ImportDeclarationKind,
    pub alias: Option<ModuleName>,
}

pub type DeclarationRef = Located<DeclarationRefKind>;

#[derive(Debug)]
pub enum NameSource {
    UserNamed,
    CompilerNamed,
}

pub type ModuleName = QualifiedName;

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
        constructors: Option<DeclarationRefConstructors>,
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
pub enum DeclarationRefConstructors {
    All,
    Some(Vec<Symbol>),
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

impl TypeDeclarationData {
    pub fn new(ident: Symbol, r#type: Type) -> Self {
        Self { ident, r#type }
    }
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

    // TODO: do we need this? seems internal
    DataBindingGroup(Vec<Declaration>),

    TypeSynonym {
        name: Symbol,
        params: Vec<(Symbol, Type)>,
        body: Type,
    },

    KindSignature {
        for_type: KindSignatureFor,
        name: Symbol,
        kind: Type,
    },

    Role(RoleDeclarationData),

    TypeSignature(TypeDeclarationData),

    ValueDeclaration(ValueDeclaration),

    ForeignValue {
        name: Symbol,
        type_: Type,
    },
}

#[derive(Debug)]
pub enum KindSignatureFor {
    Data,
    Newtype,
    TypeSynonym,
    Class,
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
    pub params: Vec<Pat>,
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
