pub mod tests {
    pub trait DropSalsaId {
        fn drop_salsa_id(&mut self) -> Self;
    }

    impl DropSalsaId for String {
        fn drop_salsa_id(&mut self) -> String {
            self.lines()
                .filter(|l| !l.contains("[salsa id]"))
                .collect::<Vec<_>>()
                .join("\n")
        }
    }

    pub fn parse_module_id(input: &str, db: &dyn crate::Db) -> crate::ModuleId {
        crate::parser::parse_module_name(input)
            .map(|name| crate::ModuleId::new(db, name))
            .expect("The input should be valid purescript module")
    }

    pub fn dummy_module(db: &dyn crate::Db) -> crate::ModuleId {
        crate::ModuleId::new(db, "Test".into())
    }
}
