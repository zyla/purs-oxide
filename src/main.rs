use anyhow::format_err;
use purs_oxide::{Diagnostic, Diagnostics};
use rayon::prelude::*;
use salsa::ParallelDatabase;
use std::{fs::File, io::Read};

fn main() -> std::io::Result<()> {
    let mut db = purs_oxide::Database::new();
    for filename in std::env::args().skip(1) {
        if let Err(err) = process_file(&mut db, &filename) {
            println!("FAIL {} error: {}", filename, err);
        }
    }
    let db = db.snapshot();
    db.module_ids().par_iter().for_each_with(
        purs_oxide::DbSnapshot(db.snapshot()),
        |db, module| {
            let db: &dyn purs_oxide::Db = &*db.0;
            let parsed_module = purs_oxide::parsed_module(db, *module);
            let accumulated: Vec<Diagnostic> =
                purs_oxide::parsed_module::accumulated::<Diagnostics>(db, *module);

            println!(
                "parsed {}, {} declarations, diagnostics: {}",
                module.name(db),
                parsed_module.ast.declarations.len(),
                accumulated
                    .into_iter()
                    .map(|d| format!("\n - {:?}", d))
                    .collect::<String>()
            );
        },
    );
    Ok(())
}

fn process_file(db: &mut purs_oxide::Database, filename: &str) -> anyhow::Result<()> {
    let mut contents = String::new();
    File::open(&filename)?.read_to_string(&mut contents)?;
    db.add_source_file(filename.into(), contents)
        .map_err(|_| format_err!("Invalid module name"))?;
    Ok(())
}
