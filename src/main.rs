use anyhow::format_err;
use std::{fmt::Display, fs::File, io::Read};

fn main() -> std::io::Result<()> {
    let mut db = purs_oxide::Database::new();
    for filename in std::env::args().skip(1) {
        if let Err(err) = process_file(&mut db, &filename) {
            println!("FAIL {} error: {}", filename, err);
        }
    }
    for module in db.module_ids() {
        purs_oxide::parsed_module(&db, module);
        println!("parsed {}", module.name(&db));
    }
    Ok(())
}

fn process_file(db: &mut purs_oxide::Database, filename: &str) -> anyhow::Result<()> {
    let mut contents = String::new();
    File::open(&filename)?.read_to_string(&mut contents)?;
    db.add_source_file(filename.into(), contents)
        .map_err(|_| format_err!("Invalid module name"))?;
    Ok(())
}
