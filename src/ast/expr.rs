use super::ValueDeclaration;
use super::{Declaration, Located, Type};
use crate::ast::QualifiedName;
use crate::string::PSChar;
use crate::string::PSString;
use crate::symbol::Symbol;
use ordered_float::OrderedFloat;
use salsa::DebugWithDb;

pub type Expr = Located<ExprKind>;

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub enum ExprKind {
    Literal(Literal<Expr>),

    /// Infix operator sequence with unknown precedence
    Infix(Box<Expr>, Vec<(InfixOp, Expr)>),

    /// Record field accessor
    Accessor(Box<Expr>, Symbol),

    RecordUpdate(Box<Expr>, RecordUpdate),

    Var(QualifiedName),

    /// Standalone operator
    Operator(InfixOp),

    DataConstructor(QualifiedName),

    App(Box<Expr>, Vec<Expr>),

    Lam(Vec<Pat>, Box<Expr>),

    Case {
        exprs: Vec<Expr>,
        branches: Vec<CaseBranch>,
    },

    If {
        cond: Box<Expr>,
        then_: Box<Expr>,
        else_: Box<Expr>,
    },

    Typed(Box<Expr>, Box<Type>),

    Let {
        decls: Vec<Declaration>,
        body: Box<Expr>,
    },

    Wildcard,

    // Pseudo-expression, used only as an intermediate value during parsing.
    RecordUpdateSuffix(RecordUpdate),

    // Pseudo-expression, used only as an intermediate value during parsing.
    NamedPat(Symbol, Box<Expr>),

    Do(Vec<DoItem>),

    Ado(Vec<DoItem>, Box<Expr>),

    Negate(Box<Expr>),

    /// Invalid expression (but we still proceed around it)
    Error,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub enum InfixOp {
    Symbol(QualifiedName),
    Backtick(Box<Expr>),
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub enum DoItem {
    Let(Vec<Declaration>),
    Expr(Expr),
    Bind(Pat, Expr),
}

type RecordUpdate = Vec<(Symbol, Expr)>;

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub enum RecordLiteralOrUpdate {
    Literal(Vec<(Symbol, Expr)>),
    Update(Vec<(Symbol, Expr)>),
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub struct CaseBranch {
    pub pats: Vec<Pat>,
    pub expr: PossiblyGuardedExpr,
}

impl From<ValueDeclaration> for CaseBranch {
    fn from(value: ValueDeclaration) -> Self {
        Self {
            pats: value.params,
            expr: value.expr,
        }
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub enum PossiblyGuardedExpr {
    Unconditional(Expr),
    Guarded(Vec<GuardedExpr>),
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub struct GuardedExpr {
    pub guards: Vec<Guard>,
    pub expr: Expr,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub enum Guard {
    Expr(Expr),
    Bind(Pat, Expr),
}

pub type Pat = Located<PatKind>;

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub enum PatKind {
    Literal(Literal<Pat>),

    /// Infix operator sequence with unknown precedence
    Infix(Box<Pat>, Vec<(QualifiedName, Pat)>),

    Var(Symbol),

    DataConstructorApp(QualifiedName, Vec<Pat>),

    Wildcard,

    Named(Symbol, Box<Pat>),

    Typed(Box<Pat>, Box<Type>),

    Error,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, DebugWithDb)]
pub enum Literal<T> {
    Integer(i64),
    Float(OrderedFloat<f64>),
    String(PSString),
    Char(PSChar),
    Boolean(bool),
    Array(Vec<T>),
    Object(Vec<(Symbol, T)>),
}

#[test]
fn test_size() {
    // Note: it was 56 before adding multi-case.
    assert_eq!(std::mem::size_of::<Expr>(), 72);
}
