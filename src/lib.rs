use dashmap::{mapref::entry::Entry, DashMap};
use derive_new::new;
use salsa::ParallelDatabase;
use std::fmt::Display;
use std::path::PathBuf;
use std::sync::Arc;

use crate::ast::{declarations, SourceSpan};

#[macro_use]
extern crate lalrpop_util;

#[salsa::jar(db = Db)]
pub struct Jar(
    crate::ModuleSource,
    crate::ModuleId,
    crate::parsed_module,
    crate::symbol::Symbol,
    crate::Diagnostics,
);

#[salsa::input]
pub struct ModuleSource {
    id: ModuleId,
    #[return_ref]
    contents: Option<(PathBuf, String)>,
}

#[salsa::accumulator]
pub struct Diagnostics(Diagnostic);

#[derive(Clone, Debug, new)]
pub struct Diagnostic {
    pub start: usize,
    pub end: usize,
    pub message: String,
    pub file: String,
}

impl Diagnostic {
    fn from<T: std::fmt::Debug, E: Display>(
        val: &lalrpop_util::ParseError<usize, T, E>,
        file: String,
    ) -> Self {
        use lalrpop_util::ParseError::*;

        match val {
            InvalidToken { location } => {
                Diagnostic::new(*location, *location, "invalid token".into(), file)
            }

            UnrecognizedEOF { location, .. } => {
                Diagnostic::new(*location, *location, "unexpected eof".into(), file)
            }

            UnrecognizedToken {
                token: (start, token, end),
                ..
            } => Diagnostic::new(
                *start,
                *end,
                format!("unrecognized token {:?}", token),
                file,
            ),
            ExtraToken {
                token: (start, token, end),
            } => Diagnostic::new(*start, *end, format!("extra token {:?}", token), file),
            User { error } => Diagnostic::new(0, 0, format!("{}", error), file),
        }
    }
}

impl<T, E> From<lalrpop_util::ParseError<usize, T, E>> for SourceSpan {
    fn from(val: lalrpop_util::ParseError<usize, T, E>) -> Self {
        use lalrpop_util::ParseError::*;

        match val {
            InvalidToken { location } => SourceSpan {
                start: location,
                end: location,
            },

            UnrecognizedEOF { location, .. } => SourceSpan {
                start: location,
                end: location,
            },

            UnrecognizedToken {
                token: (start, .., end),
                ..
            } => SourceSpan { start, end },
            ExtraToken {
                token: (start, .., end),
            } => SourceSpan { start, end },
            User { .. } => SourceSpan { start: 0, end: 0 },
        }
    }
}

#[salsa::interned]
pub struct ModuleId {
    pub name: String,
}

#[salsa::tracked]
pub fn parsed_module(db: &dyn Db, module: ModuleId) -> crate::ast::Module {
    let (filename, input) = &db.module_source(module).contents(db).as_ref().unwrap();
    let (errs, result) = crate::parser::parse_module(db, input);
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
            Diagnostics::push(
                db,
                Diagnostic::from(&err, filename.to_string_lossy().to_string()),
            );

            declarations::corrupted(db, err.into())
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
    module_sources: Arc<DashMap<String, ModuleSource>>,
}

pub struct ModuleNameNotSpecified;

impl Database {
    pub fn new() -> Self {
        let storage = Default::default();
        Self {
            storage,
            module_sources: Arc::new(DashMap::new()),
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

impl ParallelDatabase for Database {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(Self {
            storage: self.storage.snapshot(),
            module_sources: self.module_sources.clone(),
        })
    }
}

pub struct DbSnapshot(pub salsa::Snapshot<Database>);

impl Clone for DbSnapshot {
    fn clone(&self) -> Self {
        Self(self.0.snapshot())
    }
}

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod string;
pub mod symbol;
pub mod token;
