use crate::ast::Expr;
use crate::lexer;
use crate::token::Token;
use lalrpop_util::ErrorRecovery;
use lalrpop_util::ParseError;

lalrpop_mod!(pub parser);

pub fn parse(
    input: &str,
) -> (
    Vec<ErrorRecovery<usize, Token, &str>>,
    Result<Vec<Expr>, ParseError<usize, Token, lexer::Error>>,
) {
    let mut errors = vec![];
    let lexer = lexer::lex(input);
    let result = parser::ExprsParser::new().parse(&mut errors, lexer);
    (errors, result)
}

#[cfg(test)]
mod tests {
    use super::parse;
    use insta::{self, assert_debug_snapshot};

    #[test]
    fn test_basic_parse() {
        assert_debug_snapshot!(parse("1, 2"), @r###"
        (
            [],
            Ok(
                [
                    1,
                    2,
                ],
            ),
        )
        "###);
    }
}
