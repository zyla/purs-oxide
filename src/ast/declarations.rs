use super::{Commented, Located, Pat, PossiblyGuardedExpr, Type};
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
        type_: DataDeclType,
        name: Symbol,
        params: Vec<TypeParameter>,
        kind: Option<Type>,
        constructors: Vec<DataConstructorDeclaration>,
    },

    // TODO: do we need this? seems internal
    DataBindingGroup(Vec<Declaration>),

    TypeSynonym {
        name: Symbol,
        params: Vec<TypeParameter>,
        body: Type,
    },

    KindSignature {
        for_type: KindSignatureFor,
        name: Symbol,
        kind: Kind,
    },

    Role(RoleDeclarationData),

    TypeSignature(TypeDeclarationData),

    ValueDeclaration(ValueDeclaration),

    // C x y = z, used in `let`
    Destructuring {
        pat: Pat,
        expr: PossiblyGuardedExpr,
    },

    ForeignValue {
        name: Symbol,
        type_: Type,
    },

    Class(TypeClassDeclaration),

    InstanceChain(Vec<InstanceDeclaration>),

    Operator {
        associativity: Associativity,
        precedence: u8,
        name: Symbol,
        operator: Symbol,
    },
}

#[derive(Debug)]
pub enum Associativity {
    None,
    Left,
    Right,
}

pub type Kind = Type;

pub type TypeParameter = (Symbol, Option<Kind>);

#[derive(Debug)]
pub struct TypeClassDeclaration {
    pub constraints: Vec<Type>,
    pub name: Symbol,
    pub params: Vec<TypeParameter>,
    pub fundeps: Vec<Fundep>,
    pub methods: Vec<TypeDeclarationData>,
}

#[derive(Debug)]
pub struct InstanceDeclaration {
    pub constraints: Vec<Type>,
    pub instance_type: InstanceType,
    pub instance_name: Option<Symbol>,
    pub class: QualifiedName,
    pub args: Vec<Type>,
    pub body: Vec<ValueDeclaration>,
}

#[derive(Debug)]
pub struct Fundep {
    pub from: Vec<Symbol>,
    pub to: Vec<Symbol>,
}

#[derive(Debug)]
pub enum InstanceType {
    Plain,
    Derive,
    DeriveNewtype,
}

// Note: `data` and `newtype` signatures are actually declarations without constructors
#[derive(Debug)]
pub enum KindSignatureFor {
    TypeSynonym,
    Class,
}

pub type DataConstructorDeclaration = Located<Commented<DataConstructorDeclarationData>>;

#[derive(Debug)]
pub struct DataConstructorDeclarationData {
    pub name: Symbol,

    // TODO: in original AST they have names. Why? I thought datacon fields are unnamed in PS
    pub fields: Vec<Type>,
}

#[derive(Debug)]
pub enum DataDeclType {
    Data,
    ForeignData,
    Newtype,
}

#[derive(Debug)]
pub struct ValueDeclaration {
    pub ident: Symbol,

    // TODO: what is this?
    // pub name: NameKind
    pub params: Vec<Pat>,
    pub expr: PossiblyGuardedExpr,
}
