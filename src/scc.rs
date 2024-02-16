use salsa::DebugWithDb;

use crate::renamed_module::Namespace;
use crate::symbol::Symbol;

use crate::{renamed_module::DeclId, Db, ModuleId};

#[derive(PartialEq, Eq, Clone, Debug, DebugWithDb, Hash)]
pub struct SccId {
    pub namespace: Namespace,
    pub module: ModuleId,
    pub name: Symbol,
}


#[salsa::tracked]
pub fn scc_of(db: &dyn Db, decl: DeclId) -> Vec<SccId> {
   let scc = SccId {
        namespace: decl.namespace,
        module: decl.module,
        name: decl.name
    };
    vec![scc];
}
