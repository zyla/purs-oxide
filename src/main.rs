use std::io::{stdin, Read};

fn main() -> std::io::Result<()> {
    let mut input = String::new();
    stdin().lock().read_to_string(&mut input)?;
    let (errs, result) = purs_oxide::parser::parse_module(&input);
    if !errs.is_empty() {
        for err in errs {
            eprintln!("{}", err.error);
        }
        std::process::exit(1);
    }
    match result {
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
        Ok(_) => {}
    }
    Ok(())
}
