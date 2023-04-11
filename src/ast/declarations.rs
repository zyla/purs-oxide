use super::{Commented, Located, Pat, PossiblyGuardedExpr, SourceSpan, Type};
use crate::ast::QualifiedName;
use crate::symbol::Symbol;
use crate::Db;
use crate::ModuleId;
use salsa::DebugWithDb;

pub type Module = Located<Commented<ModuleInner>>;

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub struct ModuleInner {
    pub name: ModuleId,
    pub exports: Option<Vec<DeclarationRef>>,
    pub imports: Vec<Import>,
    pub declarations: Vec<Declaration>,
}

pub fn corrupted(db: &dyn Db, span: SourceSpan) -> Module {
    let corrupted = ModuleInner {
        name: ModuleId::new(db, "CorruptedModule".into()),
        exports: Option::None,
        imports: vec![],
        declarations: vec![],
    };
    Located(span, Commented(vec![], corrupted))
}

pub type Import = Located<ImportInner>;

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub struct ImportInner {
    pub module: ModuleId,
    pub kind: ImportDeclarationKind,
    pub alias: Option<ModuleId>,
}

pub type DeclarationRef = Located<DeclarationRefKind>;

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub enum NameSource {
    UserNamed,
    CompilerNamed,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
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
        name: ModuleId,
    },
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub enum DeclarationRefConstructors {
    All,
    Some(Vec<Symbol>),
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub enum ImportDeclarationKind {
    Implicit,
    Explicit(Vec<DeclarationRef>),
    Hiding(Vec<DeclarationRef>),
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub struct RoleDeclarationData {
    pub ident: Symbol,
    pub role: Vec<Role>,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub enum Role {
    Nominal,
    Representational,
    Phantom,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
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

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
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

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub enum Associativity {
    None,
    Left,
    Right,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub enum OperatorTarget {
    Type(QualifiedName),
    DataConstructor(QualifiedName),
    Value(QualifiedName),
}

pub type Kind = Type;

pub type TypeParameter = (Symbol, Option<Kind>);

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub struct TypeClassDeclaration {
    pub constraints: Vec<Type>,
    pub name: Symbol,
    pub params: Vec<TypeParameter>,
    pub fundeps: Vec<Fundep>,
    pub methods: Vec<TypeDeclarationData>,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub struct InstanceDeclaration {
    pub constraints: Vec<Type>,
    pub instance_type: InstanceType,
    pub instance_name: Option<Symbol>,
    pub class: QualifiedName,
    pub args: Vec<Type>,
    pub body: Vec<Declaration>,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub struct Fundep {
    pub from: Vec<Symbol>,
    pub to: Vec<Symbol>,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub enum InstanceType {
    Plain,
    Derive,
    DeriveNewtype,
}

// Note: `data` and `newtype` signatures are actually declarations without constructors
#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub enum KindSignatureFor {
    TypeSynonym,
    Class,
}

pub type DataConstructorDeclaration = Located<Commented<DataConstructorDeclarationData>>;

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub struct DataConstructorDeclarationData {
    pub name: Symbol,

    // TODO: in original AST they have names. Why? I thought datacon fields are unnamed in PS
    pub fields: Vec<Type>,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub enum DataDeclType {
    Data,
    ForeignData,
    Newtype,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub struct ValueDeclaration {
    pub ident: Symbol,

    // TODO: what is this?
    // pub name: NameKind
    pub params: Vec<Pat>,
    pub expr: PossiblyGuardedExpr,
}
