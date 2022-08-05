use std::io::{stdin, Read};

fn main() -> std::io::Result<()> {
    let mut input = String::new();
    stdin().lock().read_to_string(&mut input)?;
    for tok_info in purs_oxide::lexer::lex(&input) {
        match tok_info {
            Ok(tok_info) => println!("{:?}", tok_info),
            Err(err) => println!("{:?}", err),
        }
    }
    Ok(())
}
