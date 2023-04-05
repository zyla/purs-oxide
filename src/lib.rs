use dashmap::{mapref::entry::Entry, DashMap};
use std::fmt::Display;
use std::path::PathBuf;

#[macro_use]
extern crate lalrpop_util;

#[salsa::jar(db = Db)]
pub struct Jar(crate::ModuleSource, crate::ModuleId, crate::parsed_module);

#[salsa::input]
pub struct ModuleSource {
    id: ModuleId,
    #[return_ref]
    contents: Option<(PathBuf, String)>,
}

#[salsa::interned]
pub struct ModuleId {
    pub name: String,
}

#[salsa::tracked]
pub fn parsed_module(db: &dyn Db, module: ModuleId) -> crate::ast::Module {
    let (filename, input) = &db.module_source(module).contents(db).as_ref().unwrap();
    let (errs, result) = crate::parser::parse_module(input);
    if !errs.is_empty() {
        for err in errs.iter().take(1) {
            println!(
                "FAIL {} error: {}",
                filename.to_string_lossy(),
                fmt_error(&err.error)
            );
        }
    }
    match result {
        Err(err) => {
            println!(
                "FAIL {} error: {}",
                filename.to_string_lossy(),
                fmt_error(&err)
            );
            println!("{}", err);
            panic!("Errors!");
        }
        Ok(module) => module,
    }
}

fn fmt_error<T: std::fmt::Debug, E: Display>(e: &lalrpop_util::ParseError<usize, T, E>) -> String {
    use lalrpop_util::ParseError::*;
    match e {
        InvalidToken { .. } => "invalid token".into(),
        UnrecognizedEOF { .. } => "unexpected eof".into(),
        UnrecognizedToken {
            token: (_, t, _), ..
        } => format!("unrecognized token {:?}", t),
        ExtraToken { token: (_, t, _) } => format!("extra token {:?}", t),
        User { error } => format!("{}", error),
    }
}

pub trait Db: salsa::DbWithJar<Jar> {
    fn module_source(&self, module: ModuleId) -> ModuleSource;
}

#[salsa::db(Jar)]
pub struct Database {
    storage: salsa::Storage<Self>,
    module_sources: DashMap<String, ModuleSource>,
}

pub struct ModuleNameNotSpecified;

impl Database {
    pub fn new() -> Self {
        let storage = Default::default();
        Self {
            storage,
            module_sources: DashMap::new(),
        }
    }

    pub fn add_source_file(
        &mut self,
        filename: PathBuf,
        contents: String,
    ) -> Result<ModuleId, ModuleNameNotSpecified> {
        if let Some(module_name) = parser::parse_module_name(&contents) {
            let module_id = ModuleId::new(self, module_name.clone());
            let source_file = self.module_source(module_id);
            source_file
                .set_contents(self)
                .to(Some((filename, contents)));
            Ok(module_id)
        } else {
            Err(ModuleNameNotSpecified)
        }
    }

    pub fn module_ids(&self) -> Vec<ModuleId> {
        self.module_sources
            .iter()
            .map(|x| ModuleId::new(self, x.key().clone()))
            .collect()
    }
}

impl Db for Database {
    fn module_source(&self, module: ModuleId) -> ModuleSource {
        match self.module_sources.entry(module.name(self)) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => *entry.insert(ModuleSource::new(self, module, None)),
        }
    }
}

impl salsa::Database for Database {}

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod string;
pub mod symbol;
pub mod token;
