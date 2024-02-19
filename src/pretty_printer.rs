use pretty::{BoxAllocator, DocAllocator, DocBuilder};

use crate::ast::Located;
use crate::ast::{Expr, ExprKind, QualifiedName};

pub trait PrettyPrint {
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone;
}

impl PrettyPrint for Expr {
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        match **self {
            ExprKind::Var(v) => v.pretty_print(db, allocator),
            ExprKind::Lam(_, _) => {
                todo!()
            }
            _ => todo!(),
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

impl PrettyPrint for crate::ast::Type {
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use crate::ast::TypeKind::*;

        match &**self {
            Var(v) => allocator.text(v.text(db).clone()),
            TypeConstructor(v) => v.pretty_print(db, allocator),
            a => todo!("pretty_print not implemented for {a:?} type"),
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
