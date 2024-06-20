use anyhow::format_err;
use std::path::PathBuf;
use std::{fs::File, io::Read};

pub fn load_files(db: &mut crate::Database, files: Vec<PathBuf>) {
    for filename in files {
        if let Err(err) = process_file(db, filename.clone()) {
            println!("FAIL {} error: {}", filename.to_string_lossy(), err);
        }
    }
}

pub fn process_file(db: &mut crate::Database, filename: PathBuf) -> anyhow::Result<()> {
    let mut contents = String::new();
    File::open(&filename)?.read_to_string(&mut contents)?;
    db.add_source_file(filename, contents)
        .map_err(|_| format_err!("Invalid module name"))?;
    Ok(())
}

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
