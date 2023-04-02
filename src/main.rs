use lalrpop_util::ParseError;
use std::{fmt::Display, fs::File, io::Read};

fn main() -> std::io::Result<()> {
    for filename in std::env::args().skip(1) {
        let mut input = String::new();
        File::open(&filename)?.read_to_string(&mut input)?;
        let (errs, result) = purs_oxide::parser::parse_module(&input);
        if !errs.is_empty() {
            for err in errs.iter().take(1) {
                println!("FAIL {} error: {}", filename, fmt_error(&err.error));
            }
            continue;
        }
        match result {
            Err(err) => {
                println!("FAIL {} error: {}", filename, fmt_error(&err));
                println!("{}", err);
                continue;
            }
            Ok(_) => {}
        }
        println!("OK {}", filename);
    }
    Ok(())
}

fn fmt_error<T: std::fmt::Debug, E: Display>(e: &ParseError<usize, T, E>) -> String {
    use ParseError::*;
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
