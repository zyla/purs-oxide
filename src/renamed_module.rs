use crate::{Db, ModuleId};

#[salsa::tracked]
pub fn renamed_module(db: &dyn Db, module_id: ModuleId) -> () {
    let _ = crate::indexed_module::indexed_module(db, module_id);
    todo!()
}
