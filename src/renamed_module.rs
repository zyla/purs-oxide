use std::collections::{HashMap, HashSet};

use salsa::DebugWithDb;

use petgraph::{algo::tarjan_scc, prelude::DiGraph};

use crate::{
    ast::{Declaration, DeclarationRefKind, ImportDeclarationKind},
    indexed_module::IndexedModule,
    symbol::Symbol,
    Db, ModuleId, ParsedModule,
};

#[derive(PartialEq, Eq, Clone, Debug, DebugWithDb, Hash)]
pub struct DeclId {
    pub namespace: Namespace,
    pub module: ModuleId,
    pub name: Symbol,
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
    pub declarations: Vec<Declaration>,
}

#[salsa::tracked]
pub fn renamed_module(db: &dyn Db, module_id: ModuleId) -> RenamedModule {
    let _indexed = crate::indexed_module::indexed_module(db, module_id);
    let imported = crate::renamed_module::imported_decls(db, module_id);
    let exported = crate::renamed_module::exported_decls(db, module_id);
    let declarations = vec![];

    let module = crate::parsed_module(db, module_id);

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
        imported,
        exported,
        declarations,
    }
}

struct ExportedDeclExtractor<'a> {
    db: &'a dyn Db,
    module_id: ModuleId,
    exported_decls: Vec<DeclId>,
}

impl<'a> ExportedDeclExtractor<'a> {
    fn extract(&mut self, module: &ParsedModule, indexed: &IndexedModule) {
        let db = self.db;
        match &module.ast.exports {
            Some(decl_ref_kind) => {
                let mut iter = decl_ref_kind.iter().peekable();
                while let Some(ref_decl) = iter.peek().copied() {
                    use DeclarationRefKind::*;

                    match **ref_decl {
                        TypeClass { name } => {
                            self.exported_decls.push(DeclId {
                                name,
                                module: self.module_id,
                                namespace: Namespace::Class,
                            });
                            iter.next();
                        }
                        TypeOp { name } => {
                            self.exported_decls.push(DeclId {
                                name,
                                module: self.module_id,
                                namespace: Namespace::Type,
                            });
                            iter.next();
                        }
                        Type { name, .. } => {
                            self.exported_decls.push(DeclId {
                                name,
                                module: self.module_id,
                                namespace: Namespace::Type,
                            });
                            iter.next();
                        }
                        Value { name } => {
                            self.exported_decls.push(DeclId {
                                name,
                                module: self.module_id,
                                namespace: Namespace::Value,
                            });
                            iter.next();
                        }
                        ValueOp { name } => {
                            self.exported_decls.push(DeclId {
                                name,
                                module: self.module_id,
                                namespace: Namespace::Value,
                            });
                            iter.next();
                        }
                        TypeInstanceRef { name, .. } => {
                            self.exported_decls.push(DeclId {
                                name,
                                module: self.module_id,
                                namespace: Namespace::Type,
                            });
                            iter.next();
                        }
                        Module { name } => {
                            let module = crate::parsed_module(db, name);
                            let mut inner = ExportedDeclExtractor {
                                db,
                                module_id: name,
                                exported_decls: Default::default(),
                            };
                            inner.extract(&module, &indexed);
                            self.exported_decls.append(&mut inner.exported_decls);
                            iter.next();
                        }
                    }
                }
            }
            None => {
                let mut val_iter = indexed.values.keys().peekable();

                while let Some(abs_name) = val_iter.peek().copied() {
                    self.exported_decls.push(DeclId {
                        name: abs_name.name(db),
                        module: indexed.module_id,
                        namespace: Namespace::Value,
                    });
                    val_iter.next();
                }

                let mut type_iter = indexed.types.keys().peekable();

                while let Some(abs_name) = type_iter.peek().copied() {
                    self.exported_decls.push(DeclId {
                        namespace: Namespace::Type,
                        module: indexed.module_id,
                        name: abs_name.name(db),
                    });
                    type_iter.next();
                }

                let mut class_iter = indexed.classes.keys().peekable();

                while let Some(abs_name) = class_iter.peek().copied() {
                    self.exported_decls.push(DeclId {
                        namespace: Namespace::Class,
                        module: indexed.module_id,
                        name:abs_name.name(db),
                    });
                    class_iter.next();
                }
            }
        }
    }
}

#[salsa::tracked]
pub fn exported_decls(db: &dyn Db, module_id: ModuleId) -> Vec<DeclId> {
    let indexed = crate::indexed_module::indexed_module(db, module_id);
    let module = crate::parsed_module(db, module_id);

    let mut extractor = ExportedDeclExtractor {
        db,
        module_id,
        exported_decls: Default::default(),
    };

    extractor.extract(&module, &indexed);

    extractor.exported_decls
}

pub fn imported_decls(db: &dyn Db, module_id: ModuleId) -> Vec<(Option<ModuleId>, DeclId)> {
    let module = crate::parsed_module(db, module_id);

    let mut imports: Vec<(Option<ModuleId>, DeclId)> = Vec::new();

    let mut iter = module.ast.imports.iter().peekable();
    while let Some(import) = iter.peek().copied() {
        use ImportDeclarationKind::*;
        match &import.kind {
            Implicit => {
                crate::renamed_module::exported_decls(db, import.module)
                    .iter()
                    .for_each(|i| imports.push((import.alias, i.clone())));

                iter.next();
            }
            Explicit(decls) => {
                decls
                    .into_iter()
                    .map(|i| to_decl_id(import.module, &i))
                    .for_each(|i| imports.push((import.alias, i)));

                iter.next();
            }
            Hiding(decls) => {
                let excluded: HashSet<DeclId> = decls
                    .into_iter()
                    .map(|i| to_decl_id(import.module, &i))
                    .collect();
                crate::renamed_module::exported_decls(db, import.module)
                    .iter()
                    .filter(|i| !excluded.contains(i))
                    .for_each(|i| imports.push((import.alias, i.clone())));

                iter.next();
            }
        }
    }

    imports
}

fn to_decl_id(module_id: ModuleId, kind: &DeclarationRefKind) -> DeclId {
    use DeclarationRefKind::*;

    match *kind {
        TypeClass { name } => DeclId {
            name,
            module: module_id,
            namespace: Namespace::Class,
        },
        TypeOp { name } => DeclId {
            name,
            module: module_id,
            namespace: Namespace::Type,
        },
        Type { name, .. } => DeclId {
            name,
            module: module_id,
            namespace: Namespace::Type,
        },
        Value { name } => DeclId {
            name,
            module: module_id,
            namespace: Namespace::Value,
        },
        ValueOp { name } => DeclId {
            name,
            module: module_id,
            namespace: Namespace::Value,
        },
        TypeInstanceRef { name, .. } => DeclId {
            name,
            module: module_id,
            namespace: Namespace::Type,
        },
        Module { .. } => panic!("Cannot map module to DeclId"),
    }
}

#[cfg(test)]
mod tests {
    use crate::Diagnostics;

    use super::*;
    use indoc::indoc;
    use insta::{self, assert_snapshot};

    fn export_decls(input: &str) -> String {
        let db = &mut crate::Database::test_single_file_db(input);
        let module_id = ModuleId::new(db, "Test".into());

        format!(
            "{:#?}",
            (
                exported_decls(db, module_id).into_debug_all(db),
                renamed_module::accumulated::<Diagnostics>(db, module_id)
            )
        )
    }

    fn import_decls(input: &str) -> String {
        let db = &mut crate::Database::test_single_file_db(input);

        let lib = indoc!(
            "
        module Lib where
        
        data Foo = Bar
        "
        );
        db.add_source_file("lib.purs".into(), lib.into()).unwrap();

        let module_id = ModuleId::new(db, "Test".into());

        format!(
            "{:#?}",
            (
                imported_decls(db, module_id).into_debug_all(db),
                renamed_module::accumulated::<Diagnostics>(db, module_id)
            )
        )
    }

    #[test]
    fn export_data_decl() {
        assert_snapshot!(export_decls(indoc!(
            "
    module Test (Foo) where
    data Foo
    "
        )))
    }

    #[test]
    fn export_blanket_data_decl() {
        assert_snapshot!(export_decls(indoc!(
            "
    module Test where
    data Foo
    "
        )))
    }

    #[test]
    fn export_class_decl() {
        assert_snapshot!(export_decls(indoc!(
            "
    module Test where

    class Foo a where
      foo :: a
    "
        )))
    }

    #[test]
    fn import_data_decl() {
        assert_snapshot!(import_decls(indoc!(
            "
        module Test where
        
        import Foo (Foo(..))
        "
        )))
    }
    
    #[test]
    fn import_data_qualified_decl() {
        assert_snapshot!(import_decls(indoc!(
            "
        module Test where
        
        import Lib as Lib
        "
        )))
    }
}
