use crate::symbol::Symbol;

#[derive(Debug)]
pub struct SourceSpan {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug)]
pub struct Located<T>(pub SourceSpan, pub T);

#[derive(Debug)]
pub struct Comment(pub String);

#[derive(Debug)]
pub struct Commented<T>(pub Vec<Comment>, pub T);

pub type Module = Located<Commented<ModuleInner>>;

#[derive(Debug)]
pub struct ModuleInner {
    pub name: Symbol,
    pub exports: Option<Vec<DeclarationRef>>,
    pub declarations: Vec<Declaration>,
}

#[derive(Debug)]
pub enum DeclarationRef {
    // TODO
}

pub type Declaration = Located<Commented<DeclarationKind>>;

#[derive(Debug)]
pub enum DeclarationKind {
    ValueDeclaration(ValueDeclaration),
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
