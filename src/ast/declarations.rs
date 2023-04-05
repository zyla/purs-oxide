use super::{Commented, Located, Pat, PossiblyGuardedExpr, Type};
use crate::ast::QualifiedName;
use crate::symbol::Symbol;

pub type Module = Located<Commented<ModuleInner>>;

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct ModuleInner {
    pub name: ModuleName,
    pub exports: Option<Vec<DeclarationRef>>,
    pub imports: Vec<Import>,
    pub declarations: Vec<Declaration>,
}

pub type Import = Located<ImportInner>;

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct ImportInner {
    pub module: ModuleName,
    pub kind: ImportDeclarationKind,
    pub alias: Option<ModuleName>,
}

pub type DeclarationRef = Located<DeclarationRefKind>;

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum NameSource {
    UserNamed,
    CompilerNamed,
}

pub type ModuleName = QualifiedName;

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
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

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum DeclarationRefConstructors {
    All,
    Some(Vec<Symbol>),
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum ImportDeclarationKind {
    Implicit,
    Explicit(Vec<DeclarationRef>),
    Hiding(Vec<DeclarationRef>),
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct RoleDeclarationData {
    pub ident: Symbol,
    pub role: Vec<Role>,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum Role {
    Nominal,
    Representational,
    Phantom,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
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

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
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
        name: OperatorTarget,
        operator: Symbol,
    },
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum Associativity {
    None,
    Left,
    Right,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum OperatorTarget {
    Type(QualifiedName),
    DataConstructor(QualifiedName),
    Value(QualifiedName),
}

pub type Kind = Type;

pub type TypeParameter = (Symbol, Option<Kind>);

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct TypeClassDeclaration {
    pub constraints: Vec<Type>,
    pub name: Symbol,
    pub params: Vec<TypeParameter>,
    pub fundeps: Vec<Fundep>,
    pub methods: Vec<TypeDeclarationData>,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct InstanceDeclaration {
    pub constraints: Vec<Type>,
    pub instance_type: InstanceType,
    pub instance_name: Option<Symbol>,
    pub class: QualifiedName,
    pub args: Vec<Type>,
    pub body: Vec<Declaration>,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct Fundep {
    pub from: Vec<Symbol>,
    pub to: Vec<Symbol>,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum InstanceType {
    Plain,
    Derive,
    DeriveNewtype,
}

// Note: `data` and `newtype` signatures are actually declarations without constructors
#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum KindSignatureFor {
    TypeSynonym,
    Class,
}

pub type DataConstructorDeclaration = Located<Commented<DataConstructorDeclarationData>>;

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct DataConstructorDeclarationData {
    pub name: Symbol,

    // TODO: in original AST they have names. Why? I thought datacon fields are unnamed in PS
    pub fields: Vec<Type>,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum DataDeclType {
    Data,
    ForeignData,
    Newtype,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct ValueDeclaration {
    pub ident: Symbol,

    // TODO: what is this?
    // pub name: NameKind
    pub params: Vec<Pat>,
    pub expr: PossiblyGuardedExpr,
}
