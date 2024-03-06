use crate::ast::AbsoluteName;
use crate::ast::DeclarationRefConstructors;
use crate::indexed_module::TypeClassDecl;
use crate::indexed_module::TypeDecl;
use fxhash::FxHashMap;
use std::collections::{HashMap, HashSet};

use salsa::DebugWithDb;

use petgraph::{algo::tarjan_scc, prelude::DiGraph};

use crate::{
    ast::{Declaration, DeclarationRefKind, ImportDeclarationKind},
    indexed_module::{IndexedModule, ValueDecl},
    rename::rename_module,
    symbol::Symbol,
    Db, ModuleId, ParsedModule,
};

#[salsa::interned]
pub struct DeclId {
    pub namespace: Namespace,
    pub module: ModuleId,
    pub name: Symbol,
}

impl DeclId {
    fn to_absolute_name(self, db: &dyn Db) -> AbsoluteName {
        AbsoluteName::new(db, self.module(db), self.name(db))
    }
}

#[derive(PartialEq, Eq, Clone, Debug, DebugWithDb, Hash)]
pub enum Namespace {
    Class,
    Type,
    Value,
}

#[derive(PartialEq, Eq, Clone, Debug, DebugWithDb)]
pub struct RenamedModule {
    pub module_id: ModuleId,
    pub imported: Vec<(Option<ModuleId>, DeclId)>,
    pub exported: Vec<DeclId>,
    pub types: FxHashMap<AbsoluteName, TypeDecl>,
    pub values: FxHashMap<AbsoluteName, ValueDecl>,
    pub classes: FxHashMap<AbsoluteName, TypeClassDecl>,
}

#[salsa::tracked]
pub fn renamed_value_decl(db: &dyn Db, id: DeclId) -> ValueDecl {
    assert!(id.namespace(db) == Namespace::Value);
    // FIXME: we shoudn't have to clone here
    let abs_name = id.to_absolute_name(db);
    renamed_module(db, id.module(db))
        .values
        .get(&abs_name)
        .cloned()
        .unwrap_or_else(|| {
            panic!(
                "renamed_value_decl for nonexistent value {:?}",
                abs_name.into_debug_all(db)
            )
        })
}

#[salsa::tracked]
pub fn renamed_module(db: &dyn Db, module_id: ModuleId) -> RenamedModule {
    let mut indexed = crate::indexed_module::indexed_module(db, module_id);
    let mut imported = crate::renamed_module::imported_decls(db, module_id);
    let mut exported = crate::renamed_module::exported_decls(db, module_id);
    let mut diagnositics = vec![];

    let module = crate::parsed_module(db, module_id);

    rename_module(
        db,
        &mut indexed,
        &mut imported,
        &mut exported,
        &mut diagnositics,
    );

    let mut graph = DiGraph::<Declaration, ()>::new();
    let mut node_indices = HashMap::new();

    for declaration in module.ast.declarations.iter() {
        let node_index = graph.add_node(declaration.clone());
        node_indices.insert(declaration.clone(), node_index);
    }

    // TODO: Add edges between nodes based on data structure
    // for declaration in module.ast.declarations.iter() {
    //     use crate::ast::DeclarationKind;
    //     match &***declaration {
    //         // Handle each variant of DeclarationKind
    //         DeclarationKind::Data { constructors, .. } => {
    //             for constructor in constructors {
    //                 if let Some(target_node) = node_indices
    //                     .get(&DeclarationKind::ValueDeclaration(constructor.name.clone()))
    //                 {
    //                     graph.add_edge(node_indices[declaration], *target_node, ());
    //                 }
    //             }
    //         }
    //         DeclarationKind::TypeSynonym { body, .. } => todo!(),
    //     }
    // }

    let _scc = tarjan_scc(&graph);

    RenamedModule {
        module_id,
        imported: imported.clone(),
        exported: exported.clone(),
        values: indexed.values,
        types: indexed.types,
        classes: indexed.classes,
    }
}

struct ExportedDeclExtractor<'a> {
    db: &'a dyn Db,
    module_id: ModuleId,
    exported_decls: Vec<DeclId>,
    imported_decls: Vec<(Option<ModuleId>, DeclId)>,
}

impl<'a> ExportedDeclExtractor<'a> {
    fn extract(&mut self, module: &ParsedModule, indexed: &IndexedModule) {
        let db = self.db;
        match &module.ast.exports {
            Some(decl_ref_kind) => {
                for ref_decl in decl_ref_kind {
                    use DeclarationRefKind::*;

                    match &**ref_decl {
                        Module { name: qualified_as } => {
                            self.exported_decls.extend(
                                self.imported_decls
                                    .iter()
                                    .filter(|(name, _)| {
                                        name.is_some_and(|name| name == *qualified_as)
                                    })
                                    .map(|(_, decl)| *decl),
                            );
                        }
                        x => {
                            self.exported_decls
                                .extend(to_decls_id(db, self.module_id, x));
                        }
                    }
                }
            }
            None => {
                for abs_name in indexed.values.keys() {
                    self.exported_decls.push(DeclId::new(
                        db,
                        Namespace::Value,
                        indexed.module_id,
                        abs_name.name(db),
                    ));
                }

                for abs_name in indexed.types.keys() {
                    self.exported_decls.push(DeclId::new(
                        db,
                        Namespace::Type,
                        indexed.module_id,
                        abs_name.name(db),
                    ));
                }

                for abs_name in indexed.classes.keys() {
                    self.exported_decls.push(DeclId::new(
                        db,
                        Namespace::Class,
                        indexed.module_id,
                        abs_name.name(db),
                    ));
                }
            }
        }
    }
}

#[salsa::tracked]
pub fn exported_decls(db: &dyn Db, module_id: ModuleId) -> Vec<DeclId> {
    let indexed = crate::indexed_module::indexed_module(db, module_id);
    let module = crate::parsed_module(db, module_id);
    let imported = imported_decls(db, module_id);

    let mut extractor = ExportedDeclExtractor {
        db,
        module_id,
        exported_decls: Default::default(),
        imported_decls: imported,
    };

    extractor.extract(&module, &indexed);

    extractor.exported_decls
}

pub fn imported_decls(db: &dyn Db, module_id: ModuleId) -> Vec<(Option<ModuleId>, DeclId)> {
    let module = crate::parsed_module(db, module_id);

    let mut imports: Vec<(Option<ModuleId>, DeclId)> = Vec::new();

    let prim_module_id = ModuleId::new(db, "Prim".into());

    let has_prim = module
        .ast
        .imports
        .iter()
        .any(|i| i.module == prim_module_id);

    if module_id != prim_module_id && !has_prim {
        crate::renamed_module::exported_decls(db, prim_module_id)
            .iter()
            .for_each(|i| imports.push((None, *i)));
    }

    for import in &module.ast.imports {
        use ImportDeclarationKind::*;
        match &import.kind {
            Implicit => {
                crate::renamed_module::exported_decls(db, import.module)
                    .iter()
                    .for_each(|i| imports.push((import.alias, *i)));
            }
            Explicit(decls) => {
                decls
                    .iter()
                    .flat_map(|i| to_decls_id(db, import.module, i))
                    .for_each(|i| imports.push((import.alias, i)));
            }
            Hiding(decls) => {
                let excluded: HashSet<DeclId> = decls
                    .iter()
                    .map(|i| to_decl_id(db, import.module, i))
                    .collect();
                crate::renamed_module::exported_decls(db, import.module)
                    .iter()
                    .filter(|i| !excluded.contains(i))
                    .for_each(|i| imports.push((import.alias, *i)));
            }
        }
    }

    imports
}

fn to_decls_id(db: &dyn Db, module_id: ModuleId, kind: &DeclarationRefKind) -> Vec<DeclId> {
    use DeclarationRefKind::*;

    match kind {
        Type { name, constructors } => {
            let mut decls = vec![DeclId::new(db, Namespace::Type, module_id, *name)];

            if let Some(constructors) = constructors {
                match constructors {
                    DeclarationRefConstructors::Some(constructors) => {
                        for constructor_name in constructors {
                            decls.push(DeclId::new(
                                db,
                                Namespace::Value,
                                module_id,
                                *constructor_name,
                            ));
                        }
                    }
                    DeclarationRefConstructors::All => {}
                }
            }
            decls
        }
        kind => vec![to_decl_id(db, module_id, kind)],
    }
}

fn to_decl_id(db: &dyn Db, module_id: ModuleId, kind: &DeclarationRefKind) -> DeclId {
    use DeclarationRefKind::*;

    match *kind {
        TypeClass { name } => DeclId::new(db, Namespace::Class, module_id, name),
        TypeOp { name } => DeclId::new(db, Namespace::Type, module_id, name),
        Type { name, .. } => DeclId::new(db, Namespace::Type, module_id, name),
        Value { name } => DeclId::new(db, Namespace::Value, module_id, name),
        ValueOp { name } => DeclId::new(db, Namespace::Value, module_id, name),
        TypeInstanceRef { name, .. } => DeclId::new(db, Namespace::Type, module_id, name),
        Module { .. } => panic!("Cannot map module to DeclId"),
    }
}

#[cfg(test)]
mod tests {
    use crate::Diagnostics;

    use super::*;
    use crate::utils::tests::DropSalsaId;
    use indoc::indoc;
    use insta::{self, assert_snapshot};

    const LIB1: &str = indoc!(
        "
        module Lib where

        data Foo = Bar
        "
    );

    const LIB2: &str = indoc!(
        "
        module Lib2 where
         
        x = 1
        y = 2
        "
    );

    fn export_decls(input: &str, deps: Vec<&str>) -> String {
        let db = &mut crate::Database::test_single_file_db(input);
        let module_id = ModuleId::new(db, "Test".into());

        deps.into_iter().zip(1..).for_each(|(deb, i)| {
            db.add_source_file(format!("Lib{}.purs", i).into(), deb.into())
                .unwrap();
        });

        format!(
            "{:#?}",
            (
                exported_decls(db, module_id).into_debug_all(db),
                renamed_module::accumulated::<Diagnostics>(db, module_id)
            )
        )
        .drop_salsa_id()
    }

    fn import_decls(input: &str, deps: Vec<&str>) -> String {
        let db = &mut crate::Database::test_single_file_db(input);
        let module_id = ModuleId::new(db, "Test".into());
        deps.into_iter().zip(1..).for_each(|(deb, i)| {
            db.add_source_file(format!("Lib{}.purs", i).into(), deb.into())
                .unwrap();
        });

        format!(
            "{:#?}",
            (
                imported_decls(db, module_id).into_debug_all(db),
                renamed_module::accumulated::<Diagnostics>(db, module_id)
            )
        )
        .drop_salsa_id()
    }

    #[test]
    fn export_data_decl() {
        assert_snapshot!(export_decls(
            indoc!(
                "
    module Test (Foo) where
    data Foo
    "
            ),
            vec![]
        ))
    }

    #[test]
    fn export_blanket_data_decl() {
        assert_snapshot!(export_decls(
            indoc!(
                "
    module Test where
    data Foo
    "
            ),
            vec![]
        ))
    }

    #[test]
    fn reexport_subset() {
        assert_snapshot!(export_decls(
            indoc!(
                "
    module Test (module X) where
    import Lib2 (x) as X
    "
            ),
            vec![LIB2]
        ))
    }

    #[test]
    fn reexport_all() {
        assert_snapshot!(export_decls(
            indoc!(
                "
    module Test (module X) where
    import Lib2 as X
    "
            ),
            vec![LIB2]
        ))
    }

    #[test]
    fn reexport_export_list() {
        assert_snapshot!(export_decls(
            indoc!(
                "
    module Test (module X) where
    import Lib as X
    "
            ),
            vec![indoc!(
                "
            module Lib (x) where
            x = 1
            y = 2
            "
            )]
        ))
    }

    #[test]
    #[ignore = "Type classes are not yet supported"]
    fn export_class_decl() {
        assert_snapshot!(export_decls(
            indoc!(
                "
    module Test where

    class Foo a where
      foo :: a
    "
            ),
            vec![]
        ))
    }

    #[test]
    fn import_data_decl() {
        assert_snapshot!(import_decls(
            indoc!(
                "
        module Test where
        import Prim ()    
        import Foo (Foo(..))
        "
            ),
            vec![LIB1]
        ))
    }

    #[test]
    fn import_data_qualified_decl() {
        assert_snapshot!(import_decls(
            indoc!(
                "
        module Test where
        import Prim ()    
        import Lib as Lib
        "
            ),
            vec![LIB1]
        ))
    }

    #[test]
    fn import_all_fns() {
        assert_snapshot!(import_decls(
            indoc!(
                "
        module Test where
        import Prim ()    
        import Lib2
        "
            ),
            vec![LIB2]
        ))
    }

    #[test]
    fn import_subset() {
        assert_snapshot!(import_decls(
            indoc!(
                "
        module Test where
        import Prim ()    
        import Lib2 (x)
        "
            ),
            vec![LIB2]
        ))
    }

    #[test]
    fn reexport_qualified() {
        assert_snapshot!(export_decls(
            indoc!(
                "
            module Test
              ( module L
              , x ) where
            
            import Lib as L
            
            x = 1
            "
            ),
            vec![LIB1]
        ))
    }

    #[test]
    fn reexport_qualified_itself() {
        assert_snapshot!(export_decls(
            indoc!(
                "
            module Test (module L, module Test) where
            
            import Lib as L
            
            x = 1
            "
            ),
            vec![LIB1]
        ))
    }

    #[test]
    #[ignore = "how should we handle this?"]
    fn import_self() {
        assert_snapshot!(export_decls(
            indoc!(
                "
            module Test where
            import Test
            x = 1
            "
            ),
            vec![]
        ))
    }
}
