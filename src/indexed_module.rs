use crate::ast::AbsoluteName;
use crate::ast::CaseBranch;
use crate::ast::DataConstructorDeclaration;
use crate::ast::DataDeclType;
use crate::ast::Declaration;
use crate::ast::DeclarationKind;
use crate::ast::Fundep;
use crate::ast::Located;
use crate::renamed_module::DeclId;
use crate::renamed_module::Namespace;
use crate::source_span::SourceSpanOps;
use salsa::DebugWithDb;
use std::iter::Peekable;

use crate::ast::Type;
use crate::ast::TypeDeclarationData;
use crate::ast::TypeParameter;
use crate::ParsedModule;
use crate::{Db, ModuleId};
use crate::{Diagnostic, Diagnostics};
use fxhash::FxHashMap;
use std::collections::hash_map::Entry;

#[derive(PartialEq, Eq, Clone, Debug, DebugWithDb)]
pub enum TypeDecl {
    Data(DataDecl),
    Type(TypeSynonymDecl),
    TypeClass(TypeClassDecl),
}

#[derive(PartialEq, Eq, Clone, Debug, DebugWithDb)]
pub struct DataDecl {
    pub type_: DataDeclType,
    pub name: AbsoluteName,
    pub params: Vec<TypeParameter>,
    pub kind: Option<Type>,
    pub constructors: Vec<DataConstructorDeclaration>,
}

#[derive(PartialEq, Eq, Clone, Debug, DebugWithDb)]
pub struct TypeSynonymDecl {
    pub name: AbsoluteName,
    pub params: Vec<TypeParameter>,
    pub body: Type,
}

#[derive(PartialEq, Eq, Clone, Debug, DebugWithDb)]
pub struct TypeClassDecl {
    pub name: AbsoluteName,
    pub constraints: Vec<Type>,
    pub params: Vec<TypeParameter>,
    pub fundeps: Vec<Fundep>,
    pub methods: Vec<TypeDeclarationData>,
}

#[derive(PartialEq, Eq, Clone, Debug, DebugWithDb)]
pub struct ValueDecl {
    pub name: AbsoluteName,
    pub type_: Option<Type>,

    /// Empty equations means foreign import.
    /// TODO: maybe make it more explicit?
    pub equations: Vec<CaseBranch>,
}

struct ModuleIndexer<'a> {
    db: &'a dyn Db,
    // Note: Using `FxHashMap` mostly because we want deterministic order for snapshots.
    types: FxHashMap<AbsoluteName, TypeDecl>,
    values: FxHashMap<AbsoluteName, ValueDecl>,
    classes: FxHashMap<AbsoluteName, TypeClassDecl>,
    module_id: ModuleId,
    decls_ref_loc: FxHashMap<DeclId, usize>,
}

impl<'a> ModuleIndexer<'a> {
    fn index(&mut self, module: ParsedModule) {
        let db = self.db;
        let mut iter = module.ast.declarations.iter().peekable();
        while let Some(src_decl) = iter.peek().copied() {
            use DeclarationKind::*;

            let ref_loc = src_decl.0.start;

            match &***src_decl {
                Data {
                    type_,
                    name,
                    params,
                    kind,
                    constructors,
                } => {
                    let abs_name = AbsoluteName::new(db, self.module_id, *name);
                    let decl_id = abs_name.to_decl_id(db, Namespace::Type);

                    self.decls_ref_loc.insert(decl_id, ref_loc);

                    let mut params = params.clone();
                    params.to_relative_span(decl_id, ref_loc);

                    let mut constructors = constructors.clone();
                    constructors.to_relative_span(decl_id, ref_loc);

                    match self.types.entry(abs_name) {
                        Entry::Occupied(_) => {
                            Diagnostics::push(
                                db,
                                Diagnostic::new(
                                    src_decl.span(),
                                    format!("Duplicate type declaration {}", name.text(db)),
                                ),
                            );
                        }
                        Entry::Vacant(e) => {
                            e.insert(TypeDecl::Data(DataDecl {
                                type_: *type_,
                                name: abs_name,
                                params,
                                kind: kind.clone(),
                                constructors,
                            }));
                        }
                    }
                    iter.next();
                }

                TypeSynonym { name, params, body } => {
                    let abs_name = AbsoluteName::new(db, self.module_id, *name);
                    let decl_id = abs_name.to_decl_id(db, Namespace::Type);

                    match self.types.entry(abs_name) {
                        Entry::Occupied(_) => {
                            Diagnostics::push(
                                db,
                                Diagnostic::new(
                                    src_decl.span(),
                                    format!("Duplicate type declaration {}", name.text(db)),
                                ),
                            );
                        }
                        Entry::Vacant(e) => {
                            let mut params = params.clone();
                            params.to_relative_span(decl_id, ref_loc);

                            let mut body = body.clone();
                            body.to_relative_span(decl_id, ref_loc);

                            e.insert(TypeDecl::Type(TypeSynonymDecl {
                                name: abs_name,
                                params,
                                body,
                            }));
                        }
                    }
                    iter.next();
                }

                KindSignature { .. } => todo!(),

                Role(_) => todo!(),

                TypeSignature(sig) => {
                    iter.next();
                    match iter.peek() {
                        Some(x) if matches!(&****x, ValueDeclaration(_)) => {
                            self.parse_value_decl(
                                &mut iter,
                                Some(Located::new(src_decl.span(), sig.clone())),
                                ref_loc,
                            );
                        }
                        _ => {
                            Diagnostics::push(
                                db,
                                Diagnostic::new(
                                    src_decl.span(),
                                    format!(
                                        "Type signature of {} should be followed by its definition",
                                        sig.ident.text(db)
                                    ),
                                ),
                            );
                        }
                    }
                }
                ValueDeclaration(_) => {
                    self.parse_value_decl(&mut iter, None, ref_loc);
                }
                Destructuring { .. } => {
                    Diagnostics::push(
                        db,
                        Diagnostic::new(src_decl.span(), "Invalid top-level destructuring".into()),
                    );
                    iter.next();
                }
                ForeignValue { name, type_ } => {
                    let abs_name = AbsoluteName::new(db, self.module_id, *name);
                    let decl_id = abs_name.to_decl_id(db, Namespace::Value);

                    self.decls_ref_loc.insert(decl_id, ref_loc);

                    match self.values.entry(abs_name) {
                        Entry::Occupied(_) => {
                            Diagnostics::push(
                                db,
                                Diagnostic::new(
                                    src_decl.span(),
                                    format!("Duplicate value declaration {}", name.text(db)),
                                ),
                            );
                        }
                        Entry::Vacant(e) => {
                            let mut type_ = type_.clone();
                            type_.to_relative_span(decl_id, ref_loc);

                            e.insert(ValueDecl {
                                name: abs_name,
                                type_: Some(type_),
                                equations: vec![],
                            });
                        }
                    }
                    iter.next();
                }
                Class(type_class_decl) => {
                    let abs_name = AbsoluteName::new(db, self.module_id, type_class_decl.name);
                    let decl_id = abs_name.to_decl_id(db, Namespace::Class);
                    self.decls_ref_loc.insert(decl_id, ref_loc);

                    match self.classes.entry(abs_name) {
                        Entry::Occupied(_) => {
                            Diagnostics::push(
                                db,
                                Diagnostic::new(
                                    src_decl.span(),
                                    format!(
                                        "Duplicate typeclass declaration {}",
                                        type_class_decl.name.text(db)
                                    ),
                                ),
                            );
                        }
                        Entry::Vacant(e) => {
                            let mut constraints = type_class_decl.constraints.clone();
                            constraints.to_relative_span(decl_id, ref_loc);

                            let mut params = type_class_decl.params.clone();
                            params.to_relative_span(decl_id, ref_loc);

                            let mut methods = type_class_decl.methods.clone();
                            methods.to_relative_span(decl_id, ref_loc);

                            e.insert(TypeClassDecl {
                                name: abs_name,
                                constraints,
                                params,
                                fundeps: type_class_decl.fundeps.clone(),
                                methods,
                            });
                        }
                    }

                    iter.next();
                }
                InstanceChain(_) => todo!(),
                Operator { .. } => todo!(),
            }
        }
    }

    fn parse_value_decl<'m>(
        &mut self,
        iter: &mut Peekable<impl Iterator<Item = &'m Declaration>>,
        sig: Option<Located<TypeDeclarationData>>,
        reference_loc: usize,
    ) {
        let db = self.db;
        let Some(first_decl) = iter.next() else {
            panic!("parse_value_decl should be called when next is ValueDeclaration")
        };
        let DeclarationKind::ValueDeclaration(first) = &***first_decl else {
            panic!("parse_value_decl should be called when next is ValueDeclaration")
        };

        let abs_name = AbsoluteName::new(db, self.module_id, first.ident);
        let decl_id = abs_name.to_decl_id(db, Namespace::Value);

        self.decls_ref_loc.insert(decl_id, reference_loc);

        let ty = match sig {
            None => None,
            Some(sig) => {
                if sig.ident == first.ident {
                    let mut sig_ty = sig.into_inner().r#type;
                    sig_ty.to_relative_span(decl_id, reference_loc);
                    Some(sig_ty)
                } else {
                    Diagnostics::push(
                        db,
                        Diagnostic::new(
                            sig.span(),
                            format!(
                                "Type signature of {} should be followed by its definition",
                                sig.ident.text(db)
                            ),
                        ),
                    );
                    None
                }
            }
        };
        let mut equations = vec![first.clone().into()];

        while let Some(src_decl) = iter.peek() {
            let DeclarationKind::ValueDeclaration(decl) = &****src_decl else {
                break;
            };
            if decl.ident != first.ident {
                // Start of another chain of equations
                break;
            }

            let mut branch: CaseBranch = decl.clone().into();
            branch.to_relative_span(decl_id, reference_loc);

            equations.push(branch);
            iter.next();
        }

        match self.values.entry(abs_name) {
            Entry::Occupied(_) => {
                Diagnostics::push(
                    db,
                    Diagnostic::new(
                        first_decl.span(),
                        format!("Duplicate value declaration {}", first.ident.text(db)),
                    ),
                );
            }
            Entry::Vacant(e) => {
                e.insert(ValueDecl {
                    name: abs_name,
                    type_: ty,
                    equations,
                });
            }
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, DebugWithDb)]
pub struct IndexedModule {
    pub module_id: ModuleId,
    pub types: FxHashMap<AbsoluteName, TypeDecl>,
    pub values: FxHashMap<AbsoluteName, ValueDecl>,
    pub classes: FxHashMap<AbsoluteName, TypeClassDecl>,
    pub decls_ref_loc: FxHashMap<DeclId, usize>,
    pub filename: String,
}

#[salsa::tracked]
pub fn indexed_module(db: &dyn Db, module_id: ModuleId) -> IndexedModule {
    let module = crate::parsed_module(db, module_id);
    let filename = module.filename.clone();
    let raw_filename = filename.to_string_lossy().to_string();
    let mut indexer = ModuleIndexer {
        db,
        types: Default::default(),
        values: Default::default(),
        classes: Default::default(),
        module_id,
        decls_ref_loc: Default::default(),
    };
    indexer.index(module);
    IndexedModule {
        module_id,
        types: indexer.types,
        values: indexer.values,
        classes: indexer.classes,
        decls_ref_loc: indexer.decls_ref_loc,
        filename: raw_filename,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::tests::*;
    use indoc::indoc;
    use insta::{self, assert_snapshot};

    fn index_module(input: &str) -> String {
        let db = &mut crate::Database::test_single_file_db(input);
        let module_id = parse_module_id(input, db);
        format!(
            "{:#?}",
            (
                indexed_module(db, module_id).into_debug_all(db),
                indexed_module::accumulated::<Diagnostics>(db, module_id)
            )
        )
        .drop_salsa_id()
    }

    #[test]
    fn index_data_decl() {
        assert_snapshot!(index_module(indoc!(
            "
        module Test where
        data Foo = Foo | Bar
        "
        )));
    }

    #[test]
    fn duplicate_data_decl() {
        assert_snapshot!(index_module(indoc!(
            "
        module Test where
        data Foo
        data Foo
        "
        )));
    }

    #[test]
    fn type_sig_without_equations_1() {
        assert_snapshot!(index_module(indoc!(
            "
        module Test where
        foo :: Int
        "
        )));
    }

    #[test]
    fn type_sig_without_equations_2() {
        assert_snapshot!(index_module(indoc!(
            "
        module Test where
        foo :: Int
        bar = 1
        "
        )));
    }

    #[test]
    fn type_sig_without_equations_3() {
        assert_snapshot!(index_module(indoc!(
            "
        module Test where
        foo :: Int
        data Foo
        "
        )));
    }

    #[test]
    fn value_decl() {
        assert_snapshot!(index_module(indoc!(
            "
        module Test where
        foo = 1
        "
        )));
    }

    #[test]
    fn value_decl_with_signature() {
        assert_snapshot!(index_module(indoc!(
            "
        module Test where
        foo :: Int
        foo = 1
        "
        )));
    }

    #[test]
    fn value_decl_many_equations() {
        assert_snapshot!(index_module(indoc!(
            "
        module Test where
        foo 1 = 1
        foo 2 = 2
        "
        )));
    }

    #[test]
    fn type_alias() {
        assert_snapshot!(index_module(indoc!(
            "
        module Test where
        type Foo = Int
        "
        )));
    }

    #[test]
    fn type_and_data_decl_conflict() {
        assert_snapshot!(index_module(indoc!(
            "
        module Test where
        type Foo = Int
        data Foo
        "
        )));
    }

    #[test]
    fn foreign_import() {
        assert_snapshot!(index_module(indoc!(
            "
        module Test where
        foreign import foo :: Int
        "
        )));
    }

    #[test]
    fn duplicate_value() {
        assert_snapshot!(index_module(indoc!(
            "
        module Test where
        foo = 1
        bar = 2
        foo = 1
        "
        )));
    }

    #[test]
    fn typeclass() {
        assert_snapshot!(index_module(indoc!(
            "
        module Test where
        
        class Show a where
          show :: a -> String
        "
        )));
    }
}
