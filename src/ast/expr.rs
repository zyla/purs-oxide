use super::Located;
use crate::ast::QualifiedName;
use crate::symbol::Symbol;

pub type Expr = Located<ExprKind>;

#[derive(Debug)]
pub enum ExprKind {
    Literal(Literal<Expr>),

    /// Infix operator sequence with unknown precedence
    Infix(Box<Expr>, Vec<(Symbol, Expr)>),

    /// Record field accessor
    Accessor(Box<Expr>, Symbol),

    // TODO
    Var(QualifiedName),

    App(Box<Expr>, Vec<Expr>),

    Lam(Vec<Pat>, Box<Expr>),
}

pub type Pat = Located<PatKind>;

#[derive(Debug)]
pub enum PatKind {
    Literal(Literal<Pat>),

    /// Infix operator sequence with unknown precedence
    Infix(Box<Pat>, Vec<(Symbol, Pat)>),

    Var(Symbol),

    Wildcard,
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
