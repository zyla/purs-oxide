use pretty::{BoxAllocator, DocAllocator, DocBuilder};

use crate::ast::Located;
use crate::ast::{CaseBranch, ExprKind, QualifiedName};

pub type Precedence = usize;

pub const APP_PRECEDENCE: Precedence = 10;
pub const FUNCTION_TYPE_PRECEDENCE: Precedence = 0;

pub trait PrettyPrint {
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        self.pretty_print_prec(db, allocator, 0)
    }

    /// Pretty-print `self`, wrapping in parens if its precedence is less than `p`.
    fn pretty_print_prec<'b, D, A>(
        &self,
        db: &dyn crate::Db,
        allocator: &'b D,
        p: Precedence,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone;
}

pub fn parens_when<'b, D, A>(
    allocator: &'b D,
    cond: bool,
    doc: DocBuilder<'b, D, A>,
) -> DocBuilder<'b, D, A>
where
    D: DocAllocator<'b, A>,
{
    if cond {
        allocator.text("(").append(doc).append(")")
    } else {
        doc
    }
}

impl<'a, T> PrettyPrint for &'a T
where
    T: PrettyPrint,
{
    fn pretty_print_prec<'b, D, A>(
        &self,
        db: &dyn crate::Db,
        allocator: &'b D,
        p: Precedence,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        (**self).pretty_print_prec(db, allocator, p)
    }
}

impl<'a, T> PrettyPrint for &'a mut T
where
    T: PrettyPrint,
{
    fn pretty_print_prec<'b, D, A>(
        &self,
        db: &dyn crate::Db,
        allocator: &'b D,
        p: Precedence,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        (**self).pretty_print_prec(db, allocator, p)
    }
}

impl PrettyPrint for ExprKind {
    fn pretty_print_prec<'b, D, A>(
        &self,
        db: &dyn crate::Db,
        allocator: &'b D,
        p: Precedence,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        match self {
            ExprKind::Var(v) => v.pretty_print_prec(db, allocator, 0),
            ExprKind::Lam(pats, body) =>
            // TODO: add actual prettiness (line breaks etc.)
            {
                parens_when(
                    allocator,
                    p > APP_PRECEDENCE,
                    allocator
                        .text("\\")
                        .append(
                            allocator.intersperse(
                                pats.iter().map(|p| {
                                    p.pretty_print_prec(db, allocator, APP_PRECEDENCE + 1)
                                }),
                                allocator.text(" "),
                            ),
                        )
                        .append(allocator.text(" -> "))
                        .append(body.pretty_print_prec(db, allocator, 0)),
                )
            }
            ExprKind::App(f, args) => parens_when(
                allocator,
                p > APP_PRECEDENCE,
                f.pretty_print_prec(db, allocator, APP_PRECEDENCE + 1)
                    .append(allocator.text(" "))
                    .append(
                        allocator.intersperse(
                            args.iter()
                                .map(|a| a.pretty_print_prec(db, allocator, APP_PRECEDENCE + 1)),
                            allocator.text(" "),
                        ),
                    ),
            ),
            ExprKind::DataConstructor(name) => name.pretty_print_prec(db, allocator, 0),
            _ => todo!("pretty_print_prec expr {:?}", self),
        }
    }
}

impl PrettyPrint for QualifiedName {
    fn pretty_print_prec<'b, D, A>(
        &self,
        db: &dyn crate::Db,
        allocator: &'b D,
        _p: Precedence,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        let module = self
            .module(db)
            .map(|m| m.name(db))
            .map(|m| m + ".")
            .unwrap_or_default();

        allocator
            .text(module)
            .append(self.name(db).text(db).clone())
    }
}

impl<T> PrettyPrint for Located<T>
where
    T: PrettyPrint,
{
    fn pretty_print_prec<'b, D, A>(
        &self,
        db: &dyn crate::Db,
        allocator: &'b D,
        p: Precedence,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        self.1.pretty_print_prec(db, allocator, p)
    }
}

impl PrettyPrint for crate::ast::PatKind {
    fn pretty_print_prec<'b, D, A>(
        &self,
        db: &dyn crate::Db,
        allocator: &'b D,
        _p: Precedence,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use crate::ast::PatKind::*;

        match self {
            Var(v) => allocator.text(v.text(db).clone()),
            a => todo!("pretty_print_prec not implemented for Pat {a:?}"),
        }
    }
}

impl PrettyPrint for crate::ast::TypeKind {
    fn pretty_print_prec<'b, D, A>(
        &self,
        db: &dyn crate::Db,
        allocator: &'b D,
        p: Precedence,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use crate::ast::TypeKind::*;

        match self {
            Var(v) => allocator.text(v.text(db).clone()),
            Unknown(x) => allocator.text(format!("%{}", x)), // Special invalid syntax for unknowns
            TypeConstructor(v) => v.pretty_print_prec(db, allocator, 0),
            FunctionType(a, b) => parens_when(
                allocator,
                p > FUNCTION_TYPE_PRECEDENCE,
                a.pretty_print_prec(db, allocator, FUNCTION_TYPE_PRECEDENCE + 1)
                    .append(allocator.text(" -> "))
                    .append(b.pretty_print_prec(db, allocator, FUNCTION_TYPE_PRECEDENCE)),
            ),
            a => todo!("pretty_print_prec not implemented for {a:?} type"),
        }
    }
}

impl PrettyPrint for crate::indexed_module::TypeDecl {
    fn pretty_print_prec<'b, D, A>(
        &self,
        db: &dyn crate::Db,
        allocator: &'b D,
        _p: Precedence,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use crate::indexed_module::TypeDecl::*;
        match self {
            Data(d) => allocator
                .text("data ")
                .append(d.name.name(db).text(db).clone())
                .append("\n"),
            Type(t) => allocator
                .text("type ")
                .append(t.name.name(db).text(db).clone())
                // TODO: pretty print type params
                .append(allocator.text(" = "))
                .append(t.body.pretty_print_prec(db, allocator, 0))
                .append("\n"),
            TypeClass(c) => allocator
                .text("class ")
                .append(c.name.name(db).text(db).clone())
                .append("\n"),
        }
    }
}

impl PrettyPrint for crate::indexed_module::ValueDecl {
    fn pretty_print_prec<'b, D, A>(
        &self,
        db: &dyn crate::Db,
        allocator: &'b D,
        _p: Precedence,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        allocator
            .text("")
            .append(match self.type_.clone() {
                Some(t) => allocator
                    .text(self.name.name(db).text(db).clone())
                    .append(allocator.text(" :: "))
                    .append(t.pretty_print_prec(db, allocator, 0))
                    .append("\n"),
                None => allocator.text(""),
            })
            .append(allocator.intersperse(
                self.equations.iter().map(|e| {
                    allocator.text(self.name.name(db).text(db).clone()).append(
                        allocator
                            .concat(e.pats.iter().map(|p| {
                                allocator
                                    .text(" ")
                                    .append(p.pretty_print_prec(db, allocator, 0))
                            }))
                            .append(allocator.text(" = "))
                            .append(e.expr.pretty_print_prec(db, allocator, 0)),
                    )
                }),
                allocator.text("\n"),
            ))
    }
}

impl PrettyPrint for CaseBranch {
    fn pretty_print_prec<'b, D, A>(
        &self,
        _db: &dyn crate::Db,
        _allocator: &'b D,
        _p: Precedence,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        todo!()
    }
}

impl PrettyPrint for crate::ast::expr::PossiblyGuardedExpr {
    fn pretty_print_prec<'b, D, A>(
        &self,
        db: &dyn crate::Db,
        allocator: &'b D,
        _p: Precedence,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use crate::ast::expr::PossiblyGuardedExpr::*;
        match self {
            Unconditional(e) => e.pretty_print_prec(db, allocator, 0),
            Guarded(g) => todo!("pretty_print_prec not implemented for PossiblyGuardedExpr {g:?}"),
        }
    }
}

pub fn pp<T: PrettyPrint>(db: &dyn crate::Db, x: T) -> String {
    // TODO: we probably don't have to convert to a string here (instead return `impl
    // fmt::Display`), but something something borrow checker
    x.pretty_print::<_, ()>(db, &BoxAllocator)
        .pretty(80)
        .to_string()
}
