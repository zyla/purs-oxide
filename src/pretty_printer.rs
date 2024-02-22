use pretty::{BoxAllocator, DocAllocator, DocBuilder};

use crate::ast::Located;
use crate::ast::{CaseBranch, ExprKind, QualifiedName};

pub trait PrettyPrint {
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone;
}

impl<'a, T> PrettyPrint for &'a T
where
    T: PrettyPrint,
{
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        (**self).pretty_print(db, allocator)
    }
}

impl<'a, T> PrettyPrint for &'a mut T
where
    T: PrettyPrint,
{
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        (**self).pretty_print(db, allocator)
    }
}

impl PrettyPrint for ExprKind {
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        match self {
            ExprKind::Var(v) => v.pretty_print(db, allocator),
            ExprKind::Lam(pats, body) =>
            // TODO: add actual prettiness (line breaks etc.)
            {
                allocator
                    .text("\\")
                    .append(allocator.intersperse(
                        pats.iter().map(|p| p.pretty_print(db, allocator)),
                        allocator.text(" "),
                    ))
                    .append(allocator.text(" -> "))
                    .append(body.pretty_print(db, allocator))
            }
            ExprKind::App(f, args) => f
                .pretty_print(db, allocator)
                .append(allocator.text(" "))
                .append(allocator.intersperse(
                    args.iter().map(|a| a.pretty_print(db, allocator)),
                    allocator.text(" "),
                )),
            ExprKind::DataConstructor(name) => name.pretty_print(db, allocator),
            _ => todo!("pretty_print expr {:?}", self),
        }
    }
}

impl PrettyPrint for QualifiedName {
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
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
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        self.1.pretty_print(db, allocator)
    }
}

impl PrettyPrint for crate::ast::PatKind {
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use crate::ast::PatKind::*;

        match self {
            Var(v) => allocator.text(v.text(db).clone()),
            a => todo!("pretty_print not implemented for Pat {a:?}"),
        }
    }
}

impl PrettyPrint for crate::ast::TypeKind {
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use crate::ast::TypeKind::*;

        match self {
            Var(v) => allocator.text(v.text(db).clone()),
            Unknown(x) => allocator.text(format!("%{}", x)), // Special invalid syntax for unknowns
            TypeConstructor(v) => v.pretty_print(db, allocator),
            FunctionType(a, b) => {
                let pp_a = match &***a {
                    FunctionType(_, _) => allocator
                        .text("(")
                        .append(a.pretty_print(db, allocator))
                        .append(")"),
                    _ => a.pretty_print(db, allocator),
                };

                pp_a.append(allocator.text(" -> "))
                    .append(b.pretty_print(db, allocator))
            }
            a => todo!("pretty_print not implemented for {a:?} type"),
        }
    }
}

impl PrettyPrint for crate::indexed_module::TypeDecl {
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
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
                .append(t.body.pretty_print(db, allocator))
                .append("\n"),
            TypeClass(c) => allocator
                .text("class ")
                .append(c.name.name(db).text(db).clone())
                .append("\n"),
        }
    }
}

impl PrettyPrint for crate::indexed_module::ValueDecl {
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
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
                    .append(t.pretty_print(db, allocator))
                    .append("\n"),
                None => allocator.text(""),
            })
            .append(
                allocator
                    .text(self.name.name(db).text(db).clone())
                    .append(allocator.intersperse(
                        self.equations.iter().flat_map(|e| {
                            e.pats
                                .iter()
                                .map(|p| allocator.text(" ").append(p.pretty_print(db, allocator)))
                        }),
                        allocator.text(""),
                    ))
                    .append(allocator.text(" = ")),
            )
            .append(allocator.intersperse(
                self.equations.iter().map(|e| e.pretty_print(db, allocator)),
                allocator.text("\n"),
            ))
    }
}

impl PrettyPrint for CaseBranch {
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        self.expr.pretty_print(db, allocator)
    }
}

impl PrettyPrint for crate::ast::expr::PossiblyGuardedExpr {
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use crate::ast::expr::PossiblyGuardedExpr::*;
        match self {
            Unconditional(e) => e.pretty_print(db, allocator),
            Guarded(g) => todo!("pretty_print not implemented for PossiblyGuardedExpr {g:?}"),
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
