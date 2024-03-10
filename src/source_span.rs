use salsa::DebugWithDb;

use crate::{
    ast::{CaseBranch, Type, TypeDeclarationData},
    renamed_module::DeclId,
    symbol::Symbol,
    ModuleId,
};

#[derive(Eq, PartialEq, Debug, Hash, Clone, Copy, DebugWithDb)]
pub struct SourceSpan {
    pub decl: SpanDeclRef,
    pub start: usize,
    pub end: usize,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Copy, DebugWithDb)]
pub enum SpanDeclRef {
    Module(ModuleId),
    Decl(DeclId),
    Unknown,
}

impl SourceSpan {
    pub fn new_in_module(start: usize, end: usize, module_id: ModuleId) -> Self {
        Self {
            start,
            end,
            decl: SpanDeclRef::Module(module_id),
        }
    }

    pub fn new_relative(start: usize, end: usize, decl_id: DeclId) -> Self {
        Self {
            start,
            end,
            decl: SpanDeclRef::Decl(decl_id),
        }
    }

    pub fn unknown() -> Self {
        // TODO: find a better representation?
        Self {
            start: 0,
            end: 0,
            decl: SpanDeclRef::Unknown,
        }
    }

    /// Alias for `unknown()`, but we can grep for it and gradually add sensible locations
    pub fn todo() -> Self {
        Self::unknown()
    }

    pub fn to_file_location(&self, db: &dyn crate::Db) -> (String, usize, usize) {
        match self.decl {
            SpanDeclRef::Module(module) => {
                let indexed = crate::indexed_module::indexed_module(db, module);
                (indexed.filename, self.start, self.end)
            }
            SpanDeclRef::Decl(ref decl_id) => {
                let indexed = crate::indexed_module::indexed_module(db, decl_id.module(db));
                match indexed.decls_ref_loc.get(decl_id) {
                    Some(loc_ref) => (indexed.filename, loc_ref + self.start, loc_ref + self.end),
                    None => panic!("unknown decl id: {:?}", decl_id),
                }
            }
            SpanDeclRef::Unknown => ("Unknown".into(), 0, 0),
        }
    }
}

pub trait ToSourceSpan {
    fn to_source_span(&self, module_id: ModuleId) -> SourceSpan;
}

pub trait ToRelativeSourceSpan {
    fn to_relative_span(&mut self, decl_id: DeclId, reference_loc: usize) -> &Self;
}

impl ToRelativeSourceSpan for SourceSpan {
    fn to_relative_span(&mut self, decl_id: DeclId, reference_loc: usize) -> &SourceSpan {
        self.decl = SpanDeclRef::Decl(decl_id);
        self.start -= reference_loc;
        self.end -= reference_loc;
        self
    }
}

impl ToRelativeSourceSpan for (Symbol, Option<Type>) {
    fn to_relative_span(&mut self, decl_id: DeclId, reference_loc: usize) -> &Self {
        if let Some(typ) = &mut self.1 {
            typ.to_relative_span(decl_id, reference_loc);
        };
        self
    }
}

impl<A: ToRelativeSourceSpan> ToRelativeSourceSpan for Vec<A> {
    fn to_relative_span(&mut self, decl_id: DeclId, reference_loc: usize) -> &Self {
        for item in self.iter_mut() {
            item.to_relative_span(decl_id, reference_loc);
        }
        self
    }
}

impl ToRelativeSourceSpan for TypeDeclarationData {
    fn to_relative_span(&mut self, decl_id: DeclId, reference_loc: usize) -> &Self {
        self.r#type.to_relative_span(decl_id, reference_loc);
        self
    }
}

impl ToRelativeSourceSpan for CaseBranch {
    fn to_relative_span(&mut self, decl_id: DeclId, reference_loc: usize) -> &Self {
        self.pats.to_relative_span(decl_id, reference_loc);
        self
    }
}

// #[salsa::tracked]
// pub fn fetch_absolute_source_span(db: &dyn Db, decl_id: DeclId) -> SourceSpan {
//     let module_id = decl_id.modele(db);
//     let indexed = crate::indexed_module::indexed_module(db, module_id);
//     match indexed.decls_ref_loc.get(&decl_id) {
//         Some(loc_ref) => SourceSpan::new_in_module(loc_ref + start, end, module_id);
//         None => panic!("unknown decl id: {:?}", decl_id),
//     }
// }
