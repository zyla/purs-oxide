use anyhow::format_err;
use clap::Parser;
use purs_oxide::codegen::BundleMode;
use purs_oxide::{Diagnostic, Diagnostics};
use rayon::prelude::*;
use salsa::ParallelDatabase;
use std::fmt::Write;
use std::path::PathBuf;
use std::{fs::File, io::Read};

#[derive(clap::Parser)]
#[command(version, about)]
enum Command {
    Parse {
        #[arg(value_name = "FILES", help = "The input .purs file(s).")]
        files: Vec<PathBuf>,
    },
    Bundle {
        #[arg(value_name = "FILES", help = "The input .purs file(s)")]
        files: Vec<PathBuf>,
        #[arg(
            short,
            long,
            value_name = "MODULE",
            help = "Specify the qualified entrypoint function (e.g., App.main)"
        )]
        entrypoint: String,
    },
}

fn main() -> std::io::Result<()> {
    let mut db = purs_oxide::Database::new();
    match Command::parse() {
        Command::Parse { files } => {
            load_files(files, &mut db);
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
                            .fold(String::new(), |mut output, d| {
                                write!(output, "\n - {:?}", d).expect("write to String");
                                output
                            })
                    );
                },
            );
        }
        Command::Bundle { files, entrypoint } => {
            load_files(files, &mut db);

            //let (err, entry) = purs_oxide::parser::parse_lower_qualified_ident(&db, &entrypoint);

            let entrypoint = match purs_oxide::parser::parse_lower_qualified_ident(&db, &entrypoint)
                .1
                .unwrap()
                .to_absolute_name(&db)
            {
                Some(abs) => abs,
                None => panic!("invalid entrypoint {}", entrypoint),
            };
            let (code, _ffi) = purs_oxide::codegen::bundle(&db, BundleMode::Export, entrypoint);
            println!("{}", code);
        }
    }
    Ok(())
}

fn load_files(files: Vec<PathBuf>, db: &mut purs_oxide::Database) {
    for filename in files {
        if let Err(err) = process_file(db, filename.clone()) {
            println!("FAIL {} error: {}", filename.to_string_lossy(), err);
        }
    }
}

fn process_file(db: &mut purs_oxide::Database, filename: PathBuf) -> anyhow::Result<()> {
    let mut contents = String::new();
    File::open(&filename)?.read_to_string(&mut contents)?;
    db.add_source_file(filename, contents)
        .map_err(|_| format_err!("Invalid module name"))?;
    Ok(())
}
