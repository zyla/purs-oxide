use crate::ast::PatKind;
use crate::ast::{Expr, ExprKind};
use fxhash::FxHashSet;

use crate::ast::AbsoluteName;
use crate::renamed_module::DeclId;
use crate::renamed_module::Namespace;
use crate::scc::scc_of;
use crate::scc::SccId;
use crate::typecheck::typechecked_scc;
use std::fmt::Write;

/// Different ways to reference `AbsoluteName`s.
#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum ReferenceMode {
    /// Names are imported from their respective modules. `Var(AbsoluteName)` is compiled to
    /// `Some_Module.valueName`.
    ModuleImport,
    /// Names are imported from their respective modules. `Var(AbsoluteName)` is compiled to
    /// `Some_Module__valueName`.
    LocalConstant,
}

#[salsa::accumulator]
pub struct CodeAccumulator(pub String);

/// Generate code for the given value and its transitive dependencies, collecting it in the
/// `CodeAccumulator` accumulator.
#[salsa::tracked]
pub fn value_decl_code_acc(db: &dyn crate::Db, id: AbsoluteName) {
    scc_code_acc(
        db,
        scc_of(
            db,
            DeclId::new(db, Namespace::Value, id.module(db), id.name(db)),
        ),
    )
}

macro_rules! cg_write {
    ($cg:expr, $($arg:tt)*) => {
        write!(&mut $cg.code_buffer, $($arg)*).expect("formatting to a String shouldn't fail")
    };
}

/// Generate code for the given SCC and its transitive dependencies, collecting it in the
/// `CodeAccumulator` accumulator.
#[salsa::tracked]
pub fn scc_code_acc(db: &dyn crate::Db, id: SccId) {
    let mut cg = CodeGenerator {
        db,
        reference_mode: ReferenceMode::LocalConstant,
        code_buffer: String::new(),
        dependencies: Default::default(),
    };
    for (id, expr, _) in typechecked_scc(db, id) {
        cg_write!(
            cg,
            "const {}__{} = ",
            id.module(db).name(db),
            id.name(db).text(db)
        );
        cg.expr(&expr);
        cg_write!(cg, ";\n");
    }
    for dep in cg.dependencies {
        value_decl_code_acc(db, dep);
    }
    CodeAccumulator::push(db, cg.code_buffer);
}

struct CodeGenerator<'d> {
    db: &'d dyn crate::Db,
    reference_mode: ReferenceMode,
    code_buffer: String,
    dependencies: FxHashSet<AbsoluteName>,
}

impl<'d> CodeGenerator<'d> {
    fn expr(&mut self, e: &Expr) {
        let db = self.db;
        use ExprKind::*;

        match &**e {
            Var(v) => match v.to_absolute_name(db) {
                Some(abs) => {
                    self.dependencies.insert(abs);
                    match self.reference_mode {
                        ReferenceMode::ModuleImport => {
                            cg_write!(
                                self,
                                "{}.{}",
                                abs.module(db).name(db),
                                abs.name(db).text(db)
                            );
                        }
                        ReferenceMode::LocalConstant => {
                            cg_write!(
                                self,
                                "{}__{}",
                                abs.module(db).name(db),
                                abs.name(db).text(db)
                            );
                        }
                    }
                }
                None => {
                    cg_write!(self, "{}", v.name(db).text(db));
                }
            },
            App(f, xs) => {
                cg_write!(self, "(");
                self.expr(f);
                cg_write!(self, ")(");
                for (i, x) in xs.iter().enumerate() {
                    if i > 0 {
                        cg_write!(self, ", ");
                    }
                    self.expr(x);
                }
                cg_write!(self, ")");
            }
            Lam(pats, body) => {
                cg_write!(self, "(");
                for (i, p) in pats.iter().enumerate() {
                    if i > 0 {
                        cg_write!(self, ", ");
                    }
                    match &**p {
                        PatKind::Var(v) => {
                            cg_write!(self, "{}", v.text(db));
                        }
                        _ => panic!("invalid desugared lambda pattern {:?}", p),
                    }
                }
                cg_write!(self, ") => ");
                self.expr(body);
            }
            Literal(crate::ast::Literal::Integer(x)) => {
                cg_write!(self, "{}", x);
            }
            Error => {
                cg_write!(
                    self,
                    "((() => {{ throw new Error('Reached broken code') }})())",
                );
            }
            _ => todo!("codegen: unsupported expr {:?}", e),
        }
    }
}

/// Generate code for the given SCC without its dependencies. For use in multi-module code
/// generation mode.
#[salsa::tracked]
pub fn scc_code(db: &dyn crate::Db, id: SccId) -> String {
    let mut cg = CodeGenerator {
        db,
        reference_mode: ReferenceMode::ModuleImport,
        code_buffer: String::new(),
        dependencies: Default::default(),
    };
    for (id, expr, _) in typechecked_scc(db, id) {
        cg_write!(cg, "const {} = ", id.name(db).text(db));
        cg.expr(&expr);
        cg_write!(cg, ";\n");
    }
    cg.code_buffer
}

#[cfg(test)]
mod bundle_tests {
    use super::*;
    use crate::symbol::Symbol;
    use crate::ModuleId;
    use indoc::indoc;
    use insta::*;

    fn test_bundle(inputs: &[&str], entrypoint: (&str, &str)) -> String {
        let _ = env_logger::builder().is_test(true).try_init();
        let db = &mut crate::Database::new();

        inputs.into_iter().zip(1..).for_each(|(source, i)| {
            db.add_source_file(format!("Lib{}.purs", i).into(), source.to_string())
                .unwrap();
        });

        value_decl_code_acc::accumulated::<CodeAccumulator>(
            db,
            AbsoluteName::new(
                db,
                ModuleId::new(db, entrypoint.0.to_string()),
                Symbol::new(db, entrypoint.1.to_string()),
            ),
        )
        .join("")
    }

    #[test]
    fn simple1() {
        assert_snapshot!(test_bundle(
            &[indoc!(
                r"
                module Test where
                -- TODO: autoimport Prim
                import Prim (Int)
                foo :: Int
                foo = 1
                "
            )],
            ("Test", "foo")
        ));
    }

    #[test]
    fn simple_dep() {
        assert_snapshot!(test_bundle(
            &[indoc!(
                r"
                module Test where
                -- TODO: autoimport Prim
                import Prim (Int)
                unused :: Int
                unused = 1
                foo :: Int
                foo = answer
                answer :: Int
                answer = 42
                "
            )],
            ("Test", "foo")
        ));
    }

    #[test]
    #[ignore = "Can't typecheck lambda with type signature yet"]
    fn function_call() {
        assert_snapshot!(test_bundle(
            &[indoc!(
                r"
                module Test where
                -- TODO: autoimport Prim
                import Prim (Int)
                foo :: Int
                foo = frob answer
                answer :: Int
                answer = 42
                frob :: Int -> Int
                -- note: explicit lambda because no equations desugaring yet
                frob = \x -> x
                "
            )],
            ("Test", "foo")
        ));
    }
}
