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
    let foreign_path = get_foreign_file_path(&filename);
    let module_id = db
        .add_source_file(filename, contents)
        .map_err(|_| format_err!("Invalid module name"))?;

    if let Some(foreign_path) = foreign_path {
        process_foreign_file(db, module_id, foreign_path)?;
    }
    Ok(())
}

fn get_foreign_file_path(purs_path: &PathBuf) -> Option<PathBuf> {
    if let Some(ext) = purs_path.extension() {
        if ext == "purs" {
            let mut js_path = purs_path.clone();
            js_path.set_extension("js");
            if js_path.exists() {
                return Some(js_path);
            }
        }
    }
    None
}

pub fn process_foreign_file(
    db: &mut crate::Database,
    module_id: crate::ModuleId,
    filename: PathBuf,
) -> anyhow::Result<()> {
    let mut contents = String::new();
    File::open(&filename)?
        .read_to_string(&mut contents)
        .map_err(|err| {
            format_err!(
                "Foreign file {} not found: {}",
                filename.to_string_lossy(),
                err
            )
        })?;

    db.add_foreign_file(module_id, filename, contents);
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
