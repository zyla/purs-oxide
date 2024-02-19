use crate::ast::AbsoluteName;
use crate::ast::CaseBranch;
use crate::ast::Located;
use crate::ast::PatKind;
use crate::ast::PossiblyGuardedExpr;
use crate::ast::QualifiedName;
use crate::ast::TypeKind;
use crate::indexed_module::ValueDecl;
use crate::renamed_module::renamed_value_decl;
use crate::scc::{decls_in_scc, SccId};
use crate::Db;
use crate::SourceSpan;
use crate::{
    ast::{Expr, ExprKind::*, Type},
    renamed_module::DeclId,
};
use std::collections::HashMap;

#[salsa::tracked]
pub fn type_of_value(db: &dyn Db, name: AbsoluteName) -> Type {
    todo!("type_of_value")
}

#[salsa::tracked]
pub fn typechecked_scc(db: &dyn Db, scc_id: SccId) -> Vec<(DeclId, Expr, Type)> {
    // TODO: merge decls_in_scc and renamed_value_decl?
    let decls = decls_in_scc(db, scc_id)
        .into_iter()
        .map(|decl_id| (decl_id, renamed_value_decl(db, decl_id)))
        .collect::<Vec<_>>();

    let local_context = decls
        .iter()
        .map(|(_, d)| {
            (
                d.name.to_qualified_name(db),
                match &d.type_ {
                    Some(t) => t.clone(), // TODO: figure out how not to clone here
                    None => {
                        // TODO: here we should generate a fresh type
                        // variable
                        todo!("typechecking stuff without type signatures not supported")
                    }
                },
            )
        })
        .collect::<HashMap<_, _>>();

    decls
        .iter()
        .map(|(id, d)| {
            let ty = d
                .type_
                .as_ref()
                .expect("typechecking stuff without type signatures not supported");
            // FIXME: clone (x2, especially context)
            let expr = Typechecker::new(db, local_context.clone()).check(to_expr(d.clone()), ty);
            (*id, expr, ty.clone()) // FIXME: clone
        })
        .collect()
}

// FIXME: this should be done already by the desugarer (separate desugared ValueDecl)
fn to_expr(d: ValueDecl) -> Expr {
    match &d.equations[..] {
        [CaseBranch {
            pats,
            expr: PossiblyGuardedExpr::Unconditional(expr),
        }] if pats.is_empty() => expr.clone(), // FIXME: clone
        _ => panic!("ValueDecl not desugared properly"),
    }
}

struct Typechecker<'a> {
    db: &'a dyn Db,
    local_context: HashMap<QualifiedName, Type>,
    next_tv: u64,
}

impl<'a> Typechecker<'a> {
    fn new(db: &'a dyn Db, local_context: HashMap<QualifiedName, Type>) -> Self {
        Self {
            db,
            local_context,
            next_tv: 1,
        }
    }

    fn fresh_tv(&mut self) -> Type {
        let idx = self.next_tv;
        self.next_tv += 1;
        Located::new(SourceSpan::todo(), TypeKind::Unknown(idx))
    }

    fn check(&mut self, expr: Expr, ty: &Type) -> Expr {
        match *expr {
            Lam(_, _) => todo!(),
            _ => {
                let (elaborated1, inferred_ty) = self.infer(expr);
                self.check_subsumption(elaborated1, &inferred_ty, &ty)
            }
        }
    }

    /// Checks if `a` is a subtype of `b`, returns elaborated expression
    fn check_subsumption(&mut self, expr: Expr, a: &Type, b: &Type) -> Expr {
        todo!()
    }

    fn infer(&mut self, expr: Expr) -> (Expr, Type) {
        let Located(span, expr_kind) = expr;
        match expr_kind {
            Var(v) => {
                let ty = self.local_context.get(&v).cloned().unwrap_or_else(|| {
                    match v.to_absolute_name(self.db) {
                        Some(name) => type_of_value(self.db, name),
                        None => panic!("renamer left unknown local variable"),
                    }
                });
                (Located::new(span, expr_kind), ty)
            }
            Typed(expr, ty) => todo!(),
            Lam(pats, body) => {
                let tvs = pats
                    .iter()
                    .map(|pat| match **pat {
                        PatKind::Var(v) => {
                            let tv = self.fresh_tv();
                            // TODO: we're adding to local context, but never removing from it.
                            // Seems wrong...
                            self.local_context
                                .insert(QualifiedName::new_unqualified(self.db, v), tv.clone());
                            tv
                        }
                        _ => todo!("unsupported pattern {pat:?}"),
                    })
                    .collect::<Vec<_>>();
                let (elaborated, result_ty) = self.infer(*body);
                let fn_ty = tvs.into_iter().rev().fold(result_ty, |acc, tv| {
                    Located::new(
                        SourceSpan::todo(),
                        TypeKind::FunctionType(Box::new(tv), Box::new(acc)),
                    )
                });
                (Located::new(span, Lam(pats, Box::new(elaborated))), fn_ty)
            }
            App(_, _) => todo!(),
            _ => todo!("unsupported expression {expr_kind:?}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::symbol::Symbol;

    use super::*;
    use crate::pretty_printer::*;
    use insta::assert_snapshot;

    fn test_infer(context: &[(&str, &str)], expr_str: &str) -> String {
        let db: &dyn crate::Db = &crate::Database::new();
        let local_context = context
            .iter()
            .map(|(var_name, type_str)| {
                (
                    QualifiedName::new_unqualified(db, Symbol::new(db, var_name.to_string())),
                    crate::parser::parse_type(db, type_str).1.unwrap(),
                )
            })
            .collect();
        let expr = crate::parser::parse_expr(db, expr_str).1.unwrap();
        let mut tc = Typechecker::new(db, local_context);

        let (elaborated, ty) = tc.infer(expr);
        format!("{}\n{}", pp(db, elaborated), pp(db, ty))
    }

    #[test]
    fn var() {
        assert_snapshot!(test_infer(&[("foo", "Int")], "foo"), @r###"
        foo
        Int
        "###);
    }

    #[test]
    fn lam() {
        assert_snapshot!(test_infer(&[], "\\x -> x"), @r###"
        \x -> x
        %1 -> %1
        "###);
    }

    #[test]
    fn lam_multi_arg() {
        assert_snapshot!(test_infer(&[], "\\x y -> x"), @r###"
        \x y -> x
        %1 -> %2 -> %1
        "###);
    }
}
