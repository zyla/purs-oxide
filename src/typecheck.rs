use crate::ast::CaseBranch;
use crate::ast::PossiblyGuardedExpr;
use crate::indexed_module::ValueDecl;
use crate::renamed_module::renamed_value_decl;
use crate::scc::{decls_in_scc, SccId};
use crate::Db;
use crate::{
    ast::{Expr, Type},
    renamed_module::DeclId,
};
use std::collections::HashMap;

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
                d.name,
                match &d.type_ {
                    Some(t) => t,
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
            // FIXME: clone
            let expr = check(db, to_expr(d.clone()), ty);
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

fn check(db: &dyn Db, expr: Expr, ty: &Type) -> Expr {
    todo!()
}
