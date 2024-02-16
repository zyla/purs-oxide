use crate::{renamed_module::DeclId, Db};

/// A SCC is identified by its lexicographically smallest DeclId
#[salsa::interned]
pub struct SccId {
    #[return_ref]
    pub decl: DeclId,
}

#[salsa::tracked]
pub fn scc_of(db: &dyn Db, decl: DeclId) -> SccId {
    // For now we don't handle cycles at all, so we assume every declaration is in its own cycle
    SccId::new(db, decl)
}

#[salsa::tracked]
pub fn decls_in_scc(db: &dyn Db, scc_id: SccId) -> Vec<DeclId> {
    // For now we don't handle cycles at all, so we assume every declaration is in its own cycle
    vec![*scc_id.decl(db)]
}
