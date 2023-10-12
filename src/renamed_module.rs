use salsa::DebugWithDb;

use crate::{ast::DeclarationRefKind, symbol::Symbol, Db, ModuleId, ParsedModule};

#[derive(PartialEq, Eq, Clone, Debug, DebugWithDb)]
pub struct DeclId {
    pub namespace: Namespace,
    pub module: ModuleId,
    pub name: Symbol,
}

#[derive(PartialEq, Eq, Clone, Debug, DebugWithDb)]
pub enum Namespace {
    Class,
    Type,
    Value,
}

#[derive(PartialEq, Eq, Clone, Debug, DebugWithDb)]
pub struct RenamedModule {
    pub module_id: ModuleId,
    pub imported: Vec<(Option<Symbol>, DeclId)>,
}

#[salsa::tracked]
pub fn renamed_module(db: &dyn Db, module_id: ModuleId) -> () {
    let _ = crate::indexed_module::indexed_module(db, module_id);
    todo!()
}

struct ExportedDeclExtractor {
    module_id: ModuleId,
    exported_decls: Vec<DeclId>,
}

impl ExportedDeclExtractor {
    fn extract(&mut self, module: &ParsedModule) {
        match &module.ast.exports {
            Some(decl_ref_kind) => {
                let mut iter = decl_ref_kind.iter().peekable();
                while let Some(ref_decl) = iter.peek().copied() {
                    use DeclarationRefKind::*;

                    match **ref_decl {
                        TypeClass { name } => {
                            self.exported_decls.push(DeclId {
                                name,
                                module: self.module_id.clone(),
                                namespace: Namespace::Class,
                            });
                            iter.next();
                        }
                        TypeOp { name } => {
                            self.exported_decls.push(DeclId {
                                name,
                                module: self.module_id.clone(),
                                namespace: Namespace::Type,
                            });
                            iter.next();
                        }
                        Type { name, .. } => {
                            self.exported_decls.push(DeclId {
                                name,
                                module: self.module_id.clone(),
                                namespace: Namespace::Type,
                            });
                            iter.next();
                        }
                        Value { name } => {
                            self.exported_decls.push(DeclId {
                                name,
                                module: self.module_id.clone(),
                                namespace: Namespace::Value,
                            });
                            iter.next();
                        }
                        ValueOp { name } => {
                            self.exported_decls.push(DeclId {
                                name,
                                module: self.module_id.clone(),
                                namespace: Namespace::Value,
                            });
                            iter.next();
                        }
                        TypeInstanceRef { name, .. } => {
                            self.exported_decls.push(DeclId {
                                name,
                                module: self.module_id.clone(),
                                namespace: Namespace::Type,
                            });
                            iter.next();
                        }
                        Module { name } => todo!("re-export"),
                    }
                }
            }
            None => todo!(),
        }
    }
}

#[salsa::tracked]
pub fn exported_decls(db: &dyn Db, module_id: ModuleId) -> Vec<DeclId> {
    let module = crate::parsed_module(db, module_id);

    let mut extractor = ExportedDeclExtractor {
        module_id,
        exported_decls: Default::default(),
    };

    extractor.extract(&module);

    extractor.exported_decls
}
