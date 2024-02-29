use crate::ast::Literal;
use crate::pretty_printer::pp;
use crate::symbol::Symbol;
use crate::ModuleId;
use salsa::DebugWithDb;

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
    substitution: HashMap<u64, Type>,
}

impl<'a> Typechecker<'a> {
    fn new(db: &'a dyn Db, local_context: HashMap<QualifiedName, Type>) -> Self {
        Self {
            db,
            local_context,
            next_tv: 1,
            substitution: HashMap::new(),
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
                self.check_subsumption(elaborated1, inferred_ty, ty.clone()) // TODO: clone?
            }
        }
    }

    /// Checks if `a` is a subtype of `b`, returns elaborated expression
    fn check_subsumption(&mut self, expr: Expr, mut a: Type, mut b: Type) -> Expr {
        log::debug!(
            "check_subsumption({}, {})",
            pp(self.db, &a),
            pp(self.db, &b)
        );
        // TODO: this is not really unification, it's a separate relation ("Figure 9. Algorithmic
        // subtyping" in the "very easy" paper)
        self.unify(&mut a, &mut b);
        expr
    }

    fn infer(&mut self, expr: Expr) -> (Expr, Type) {
        let db = self.db;
        let Located(span, expr_kind) = expr;
        match expr_kind {
            Var(v) => {
                let ty = self.local_context.get(&v).cloned().unwrap_or_else(|| {
                    match v.to_absolute_name(db) {
                        Some(name) => type_of_value(db, name),
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
                                .insert(QualifiedName::new_unqualified(db, v), tv.clone());
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
            App(mut f, mut xs) => {
                assert!(!xs.is_empty());
                // Note: maybe Vec of args here isn't the best idea, maybe we should have a
                // single-arg App.
                // Converting to single-arg App.
                if xs.len() > 1 {
                    f = Box::new(Located::new(span, App(f, vec![xs.remove(0)])));
                }
                let x = xs.remove(0);
                let (elaborated_f, mut f_ty) = self.infer(*f);
                let arg_ty = self.fresh_tv();
                let result_ty = self.fresh_tv();
                self.unify(
                    &mut f_ty,
                    &mut Located::new(
                        SourceSpan::todo(),
                        TypeKind::FunctionType(
                            Box::new(arg_ty.clone()),
                            Box::new(result_ty.clone()),
                        ),
                    ),
                );
                log::debug!("infer(App) checks arg");
                let elaborated_x = self.check(x, &arg_ty);
                log::debug!("infer(App) returns {}", pp(db, &result_ty));
                (
                    Located::new(span, App(Box::new(elaborated_f), vec![elaborated_x])),
                    result_ty,
                )
            }
            expr @ Literal(Literal::Integer(_)) => (
                Located::new(span, expr),
                Located::new(
                    span,
                    TypeKind::TypeConstructor(
                        // TODO: wired-in names should be shared somewhere (in Db perhaps?)
                        QualifiedName::new_qualified(
                            db,
                            ModuleId::new(db, "Prim".to_string()),
                            Symbol::new(db, "Int".to_string()),
                        ),
                    ),
                ),
            ),
            _ => todo!("unsupported expression {expr_kind:?}"),
        }
    }

    pub fn apply_subst(&mut self, t: &mut Type) {
        self.unify(t, &mut t.clone()); // TODO: don't clone...
    }

    fn unify(&mut self, t1: &mut Type, t2: &mut Type) {
        log::debug!("unify({}, {})", pp(self.db, &t1), pp(self.db, &t2));
        self.shallow_apply_subst(t1);
        self.shallow_apply_subst(t2);
        match (&mut **t1, &mut **t2) {
            (TypeKind::Unknown(u1), TypeKind::Unknown(u2)) if *u1 == *u2 => {
                // Already unified
            }
            (TypeKind::Unknown(u), _) => {
                self.occurs_check(*u, &t2);
                self.substitution.insert(*u, t2.clone());
            }
            (_, TypeKind::Unknown(u)) => {
                self.occurs_check(*u, &t1);
                self.substitution.insert(*u, t1.clone());
            }
            (
                TypeKind::FunctionType(ref mut f1, ref mut x1),
                TypeKind::FunctionType(ref mut f2, ref mut x2),
            ) => {
                self.unify(f1, f2);
                self.unify(x1, x2);
            }
            (TypeKind::TypeConstructor(c1), TypeKind::TypeConstructor(c2)) => {
                if *c1 != *c2 {
                    todo!(
                        "report error: can't unify {:?} and {:?}",
                        c1.into_debug_all(self.db),
                        c2.into_debug_all(self.db)
                    );
                }
            }
            _ => todo!(),
        }
    }

    fn occurs_check(&self, u: u64, t: &Type) {
        match &**t {
            TypeKind::Unknown(u2) => {
                if u == *u2 {
                    // Note: we shouldn't trivially continue after this error,
                    // a loop in substitution can cause typechecker nontermination
                    todo!("report occurs check error");
                }
            }
            TypeKind::FunctionType(f, x) => {
                self.occurs_check(u, &f);
                self.occurs_check(u, &x);
            }
            TypeKind::TypeConstructor(_) => {}
            _ => todo!("occurs_check: type {t:?}"),
        }
    }

    fn shallow_apply_subst(&self, t: &mut Type) {
        match &**t {
            TypeKind::Unknown(u) => {
                if let Some(t2) = self.substitution.get(u) {
                    *t = t2.clone();
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::symbol::Symbol;

    use super::*;
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

        let (elaborated, mut ty) = tc.infer(expr);
        tc.apply_subst(&mut ty);
        format!("{}\n{}", pp(db, elaborated), pp(db, ty))
    }

    fn test_check(context: &[(&str, &str)], expr_str: &str, type_str: &str) -> String {
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
        let ty = crate::parser::parse_type(db, type_str).1.unwrap();
        let mut tc = Typechecker::new(db, local_context);

        let elaborated = tc.check(expr, &ty);
        pp(db, elaborated)
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

    #[test]
    fn simple_app() {
        assert_snapshot!(test_infer(&[
                                    ("f", "Int -> String"),
                                    ("x", "Int"),
        ], "f x"), @r###"
        f x
        String
        "###);
    }

    #[test]
    fn lam_app_1() {
        assert_snapshot!(test_infer(&[
        ], "\\f x -> f x"), @r###"
        \f x -> f x
        (%3 -> %4) -> %3 -> %4
        "###);
    }

    #[test]
    #[ignore = "Error reporting not implemented"]
    fn arg_type_mismatch() {
        assert_snapshot!(test_infer(&[
                                    ("f", "Int -> String"),
                                    ("x", "Bool"),
        ], "f x"), @r###"
        f x
        String
        (Error: cannot unify Bool with Int or something)
        "###);
    }

    #[test]
    fn infer_integer_literal() {
        assert_snapshot!(test_infer(&[
        ], "1"), @r###"
        1
        Prim.Int
        "###);
    }

    #[test]
    fn check_integer_literal() {
        assert_snapshot!(test_check(&[], "1", "Prim.Int"), @"1");
    }
}
