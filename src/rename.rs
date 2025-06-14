use crate::ast::*;
use crate::indexed_module::IndexedModule;
use crate::indexed_module::{TypeClassDecl, TypeDecl, ValueDecl};
use crate::renamed_module::DeclId;
use crate::symbol::Symbol;
use crate::ModuleId;
use crate::{Db, Diagnostic};
use std::collections::{HashMap, HashSet};

pub fn rename_module(
    db: &dyn Db,
    module: &mut IndexedModule,
    imported_decls: &mut [(Option<ModuleId>, DeclId)],
    exported_decls: &mut [DeclId],
    diagnostics: &mut Vec<Diagnostic>,
) {
    let exported = exported_decls.iter().map(|decl_id| {
        (
            QualifiedName::new(db, Option::None, decl_id.name(db)),
            AbsoluteName::new(db, decl_id.module(db), decl_id.name(db)),
        )
    });

    let module_scope = imported_decls
        .iter()
        .map(|(qualified_as, id)| {
            (
                QualifiedName::new(db, *qualified_as, id.name(db)),
                AbsoluteName::new(db, id.module(db), id.name(db)),
            )
        })
        .chain(exported)
        .collect::<HashMap<_, _>>();
    let mut r = Renamer {
        db,
        module_scope,
        local_scopes: vec![HashSet::new()],
        diagnostics,
    };
    module.rename(&mut r);
}

struct Renamer<'db> {
    db: &'db dyn Db,
    /// Maps from names as appear in source code to actual absolute names
    module_scope: HashMap<QualifiedName, AbsoluteName>,
    local_scopes: Vec<HashSet<Symbol>>,
    diagnostics: &'db mut Vec<Diagnostic>,
}

impl<'db> Renamer<'db> {
    fn push_scope(&mut self) {
        self.local_scopes.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        let scope = self.local_scopes.pop();
        assert!(scope.is_some(), "pop_scope called when there are no scopes");
    }

    fn top_scope(&mut self) -> &mut HashSet<Symbol> {
        self.local_scopes
            .last_mut()
            .expect("top_scope called when there are no scopes")
    }
}

trait Rename {
    fn rename(&mut self, r: &mut Renamer);
}

impl<T> Rename for Option<T>
where
    T: Rename,
{
    fn rename(&mut self, r: &mut Renamer) {
        match self {
            None => {}
            Some(x) => x.rename(r),
        }
    }
}

impl Rename for IndexedModule {
    fn rename(&mut self, r: &mut Renamer) {
        self.values.iter_mut().for_each(|(_, v)| {
            v.rename(r);
        });

        self.types.iter_mut().for_each(|(_, v)| {
            v.rename(r);
        });

        self.classes.iter_mut().for_each(|(_, v)| {
            v.rename(r);
        });
    }
}

impl Rename for ValueDecl {
    fn rename(&mut self, r: &mut Renamer) {
        self.type_.rename(r);
        self.equations.iter_mut().for_each(|x| x.rename(r));
    }
}

impl Rename for TypeDecl {
    fn rename(&mut self, r: &mut Renamer) {
        match self {
            Self::Data(data) => {
                data.constructors.iter_mut().for_each(|constructor| {
                    for ref mut field in &mut constructor.1 .1.fields {
                        field.rename(r);
                    }
                });
            }
            Self::Type(alias) => {
                alias.body.rename(r);
            }
            Self::TypeClass(type_class) => {
                type_class.rename(r);
            }
        }
    }
}

impl Rename for TypeClassDecl {
    fn rename(&mut self, _r: &mut Renamer) {
        // TODO: rename type classes
    }
}

impl Rename for Type {
    fn rename(&mut self, r: &mut Renamer) {
        let type_ = &mut self.1;
        let db = r.db;
        match type_ {
            TypeKind::TypeConstructor(name) => match r.module_scope.get(name) {
                Some(abs) => {
                    *name = abs.to_qualified_name(db);
                }
                None => r.diagnostics.push(Diagnostic::new(
                    self.0,
                    format!("Unknown type '{}'", name.name(db).text(db)),
                )),
            },
            TypeKind::FunctionType(ref mut a, ref mut b) => {
                a.rename(r);
                b.rename(r);
            }
            TypeKind::TypeApp(ref mut f, ref mut arg) => {
                f.rename(r);
                arg.rename(r);
            }
            _ => todo!("renaming TypeKind {:?} not supported", self),
        }
    }
}

impl Rename for CaseBranch {
    fn rename(&mut self, r: &mut Renamer) {
        r.push_scope();
        for ref mut pat in &mut self.pats {
            pat.rename(r);
        }
        self.expr.rename(r);
        r.pop_scope();
    }
}

impl Rename for PossiblyGuardedExpr {
    fn rename(&mut self, r: &mut Renamer) {
        match self {
            Self::Unconditional(ref mut e) => e.rename(r),
            Self::Guarded(_) => todo!("Pattern guards not implemented"),
        }
    }
}

impl Rename for Located<PatKind> {
    fn rename(&mut self, r: &mut Renamer) {
        let pat = &mut self.1;
        match pat {
            PatKind::Var(v) => {
                if !r.top_scope().insert(*v) {
                    r.diagnostics.push(Diagnostic::new(
                        self.0,
                        format!("Duplicate variable '{}' in pattern", v.text(r.db)),
                    ));
                }
            }
            _ => todo!("renaming PatKind {:?} not supported", self),
        }
    }
}

impl Rename for Located<ExprKind> {
    fn rename(&mut self, r: &mut Renamer) {
        let expr = &mut self.1;
        match expr {
            ExprKind::Var(ref mut v) => {
                let db = r.db;
                let local_vars = r.top_scope();
                let is_local = v.module(db).is_none() && local_vars.contains(&v.name(db));
                if !is_local {
                    match r.module_scope.get(v) {
                        None => r.diagnostics.push(Diagnostic::new(
                            self.0,
                            format!("Unknown variable '{}'", v.name(db).text(db)),
                        )),
                        Some(abs) => {
                            *v = abs.to_qualified_name(db);
                            use salsa::DebugWithDb;
                            eprintln!("replacing with {:?}", v.into_debug_all(db));
                        }
                    }
                }
            }
            ExprKind::Lam(ref mut pats, ref mut expr) => {
                r.push_scope();
                for ref mut pat in pats {
                    pat.rename(r);
                }
                expr.rename(r);
                r.pop_scope();
            }
            ExprKind::App(ref mut expr, ref mut exprs) => {
                expr.rename(r);
                for ref mut expr in exprs {
                    expr.rename(r);
                }
            }
            ExprKind::Literal(_) => {}
            ExprKind::DataConstructor(constructor_name) => {
                let db = r.db;
                match r.module_scope.get(constructor_name) {
                    None => r.diagnostics.push(Diagnostic::new(
                        self.0,
                        format!(
                            "Unknown data constructor variable '{}'",
                            constructor_name.name(db).text(db)
                        ),
                    )),

                    Some(abs) => {
                        *constructor_name = abs.to_qualified_name(db);
                    }
                }
            }
            _ => todo!("renaming ExprKind {:?} not supported", self),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::pretty_printer::*;
    use indoc::indoc;
    use insta::{self, assert_snapshot};

    fn rename_mod(input: &str, deps: Vec<&str>) -> String {
        let db = &mut crate::Database::test_single_file_db(input);
        let module_id = ModuleId::new(db, "Test".into());

        deps.into_iter().zip(1..).for_each(|(deb, i)| {
            db.add_source_file(format!("Lib{}.purs", i).into(), deb.into())
                .unwrap();
        });

        let mut module = crate::indexed_module::indexed_module(db, module_id);
        let mut imported = crate::renamed_module::imported_decls(db, module_id);
        let mut exported = crate::renamed_module::exported_decls(db, module_id);
        let mut diagnostics = vec![];

        rename_module(
            db,
            &mut module,
            &mut imported,
            &mut exported,
            &mut diagnostics,
        );

        let types = module
            .types
            .values()
            .map(|v| format!("{}", pp(db, v.clone())))
            .chain(
                module
                    .values
                    .values()
                    .map(|v| format!("{}", pp(db, v.clone()))),
            )
            .collect::<Vec<_>>()
            .join("\n\n");

        format!("{}\n\n{:?}", types, diagnostics)
    }

    #[test]
    fn smoke() {
        assert_snapshot!(rename_mod(
            indoc!(
                "
        module Test where
        
        f a = a
        "
            ),
            vec![]
        ))
    }

    #[test]
    fn some_modules() {
        assert_snapshot!(rename_mod(
            indoc!(
                "
        module Test where 
        
        import Lib
        import Lib2 

        g :: A -> B -> A
        g a b = f a b

        h :: A
        h = g a b
        "
            ),
            vec![
                indoc!(
                    "
        module Lib where
        
        data A = A
        data B = B

        a :: A
        a = A

        b :: B
        b = B"
                ),
                indoc!(
                    "
        module Lib2 where
                
        import Lib        

        f :: A -> B -> A
        f a _ = a
        "
                )
            ]
        ))
    }

    #[test]
    fn duplicate_var_in_pattern() {
        assert_snapshot!(rename_mod(
            indoc!(
                "
        module Test where
        
        f a a = a
        "
            ),
            vec![]
        ))
    }

    #[test]
    fn unknown_var() {
        assert_snapshot!(rename_mod(
            indoc!(
                "
        module Test where
        
        g = x
        "
            ),
            vec![]
        ))
    }

    #[test]
    fn rename_types() {
        assert_snapshot!(rename_mod(
            indoc!(
                "module Test where

                import Lib (A(MkA))

                a :: A
                a = MkA
            "
            ),
            vec![indoc!(
                "
                module Lib where

                data A = MkA
                "
            )]
        ))
    }

    #[test]
    fn rename_type_alias() {
        assert_snapshot!(rename_mod(
            indoc!(
                "module Test where
                import Lib
                
                type B = A
            "
            ),
            vec![indoc!(
                "module Lib where
                
                data A
            "
            )]
        ))
    }

    #[test]
    fn prim_explicit_blanket() {
        assert_snapshot!(rename_mod(
            indoc!(
                "module Test where
                import Prim
                type X = Int
            "
            ),
            vec![]
        ))
    }

    #[test]
    fn prim_explicit() {
        assert_snapshot!(rename_mod(
            indoc!(
                "module Test where
                import Prim (Int)
                type X = Int
            "
            ),
            vec![]
        ))
    }

    #[test]
    fn prim_implicit() {
        assert_snapshot!(rename_mod(
            indoc!(
                "module Test where
                type X = Int
            "
            ),
            vec![]
        ))
    }

    #[test]
    #[ignore = "How should this even work?"]
    fn prim_shadowed() {
        assert_snapshot!(rename_mod(
            indoc!(
                "module Test where
                import Lib (Int)
                type X = Int
            "
            ),
            vec![indoc!(
                "module Lib where
            import Prim (String)
            type Int = String
            "
            )]
        ))
    }
}
