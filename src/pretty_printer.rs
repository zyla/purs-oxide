use crate::ast::Expr;
use crate::ast::Literal;
use crate::ast::Pat;
use crate::ast::Type;
use pretty::{BoxAllocator, DocAllocator, DocBuilder};

use crate::ast::Located;
use crate::ast::{ExprKind, QualifiedName};

pub type Precedence = usize;

pub const APP_PRECEDENCE: Precedence = 10;
pub const FUNCTION_TYPE_PRECEDENCE: Precedence = 0;

pub trait PrettyPrint {
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
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

impl PrettyPrint for Expr {
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        pretty_print_expr(self, db, allocator, 0)
    }
}

fn pretty_print_expr<'b, D, A>(
    e: &Expr,
    db: &dyn crate::Db,
    allocator: &'b D,
    p: Precedence,
) -> DocBuilder<'b, D, A>
where
    D: DocAllocator<'b, A>,
    D::Doc: Clone,
    A: Clone,
{
    match &**e {
        ExprKind::Var(v) => v.pretty_print(db, allocator),
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
                            pats.iter().map(|pat| {
                                pretty_print_pat(pat, db, allocator, APP_PRECEDENCE + 1)
                            }),
                            allocator.text(" "),
                        ),
                    )
                    .append(allocator.text(" -> "))
                    .append(pretty_print_expr(body, db, allocator, 0)),
            )
        }
        ExprKind::App(f, args) => parens_when(
            allocator,
            p > APP_PRECEDENCE,
            pretty_print_expr(f, db, allocator, APP_PRECEDENCE + 1)
                .append(allocator.text(" "))
                .append(
                    allocator.intersperse(
                        args.iter()
                            .map(|a| pretty_print_expr(a, db, allocator, APP_PRECEDENCE + 1)),
                        allocator.text(" "),
                    ),
                ),
        ),
        ExprKind::DataConstructor(name) => name.pretty_print(db, allocator),
        ExprKind::Literal(Literal::Integer(i)) => allocator.as_string(i),
        _ => todo!("pretty_print expr {:?}", e),
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

impl PrettyPrint for Pat {
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        pretty_print_pat(self, db, allocator, 0)
    }
}

fn pretty_print_pat<'b, D, A>(
    pat: &Pat,
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

    match &**pat {
        Var(v) => allocator.text(v.text(db).clone()),
        a => todo!("pretty_print not implemented for Pat {a:?}"),
    }
}

impl PrettyPrint for Type {
    fn pretty_print<'b, D, A>(&self, db: &dyn crate::Db, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        pretty_print_type(self, db, allocator, 0)
    }
}

fn pretty_print_type<'b, D, A>(
    ty: &Type,
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

    match &**ty {
        Var(v) => allocator.text(v.text(db).clone()),
        Unknown(x) => allocator.text(format!("%{}", x)), // Special invalid syntax for unknowns
        TypeConstructor(v) => v.pretty_print(db, allocator),
        FunctionType(a, b) => parens_when(
            allocator,
            p > FUNCTION_TYPE_PRECEDENCE,
            pretty_print_type(a, db, allocator, FUNCTION_TYPE_PRECEDENCE + 1)
                .append(allocator.text(" -> "))
                .append(pretty_print_type(
                    b,
                    db,
                    allocator,
                    FUNCTION_TYPE_PRECEDENCE,
                )),
        ),
        TypeApp(a, b) => parens_when(
            allocator,
            p > APP_PRECEDENCE,
            pretty_print_type(a, db, allocator, APP_PRECEDENCE)
                .append(allocator.text(" "))
                .append(pretty_print_type(b, db, allocator, APP_PRECEDENCE + 1)),
        ),

        a => todo!("pretty_print not implemented for {a:?} type"),
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
            .append(allocator.intersperse(
                self.equations.iter().map(|e| {
                    allocator.text(self.name.name(db).text(db).clone()).append(
                        allocator
                            .concat(
                                e.pats.iter().map(|p| {
                                    allocator.text(" ").append(p.pretty_print(db, allocator))
                                }),
                            )
                            .append(allocator.text(" = "))
                            .append(e.expr.pretty_print(db, allocator)),
                    )
                }),
                allocator.text("\n"),
            ))
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

#[cfg(test)]
mod tests {
    use super::*;

    fn pp_type(input: &str) -> String {
        let db = &crate::Database::new();
        pp(db, crate::parser::parse_type(db, input).1.unwrap())
    }

    fn test_pp_type_roundtrip(input: &str) {
        assert_eq!(input, pp_type(input));
    }

    #[test]
    fn function_type() {
        test_pp_type_roundtrip("a -> b -> c");
        test_pp_type_roundtrip("a -> b -> c -> d");
        test_pp_type_roundtrip("(a -> b) -> c -> d");
        test_pp_type_roundtrip("((a -> b) -> c -> d) -> e -> f");
    }

    #[test]
    fn app() {
        test_pp_type_roundtrip("Either a b");
    }

    #[test]
    fn function_type_in_app() {
        test_pp_type_roundtrip("Maybe (a -> b -> c)");
    }

    #[test]
    fn app_in_function_type() {
        test_pp_type_roundtrip("Maybe a -> Maybe b");
    }
}
