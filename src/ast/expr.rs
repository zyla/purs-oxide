use super::Located;
use crate::ast::QualifiedName;
use crate::symbol::Symbol;

pub type Expr = Located<ExprKind>;

#[derive(Debug)]
pub enum ExprKind {
    Literal(Literal<Expr>),

    /// Infix operator sequencec with unknown precedence
    Infix(Box<Expr>, Vec<(Symbol, Expr)>),

    /// Record field accessor
    Accessor(Box<Expr>, Symbol),

    // TODO
    Var(QualifiedName),
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
