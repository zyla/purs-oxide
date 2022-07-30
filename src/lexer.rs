use std::collections::VecDeque;

use crate::token::{Token, TokenInfo};

#[derive(PartialEq, Eq, Debug)]
pub struct Error(pub String);

pub fn lex(input: &str) -> impl Iterator<Item = LexResult> + '_ {
    Lexer {
        input: input.as_bytes(),
        pos: 0,
        token_start: 0,
        whitespace_start: 0,
        has_newline: false,
        queue: Default::default(),
        last_token: None,
        indent_level: 0,
        line_start: 0,
        layout_stack: Default::default(),
    }
}

struct Lexer<'a> {
    input: &'a [u8],
    pos: usize,
    whitespace_start: usize,
    token_start: usize,
    has_newline: bool,
    queue: VecDeque<TokenInfo>,
    last_token: Option<TokenInfo>,
    /** stack of indent levels */
    layout_stack: Vec<usize>,
    line_start: usize,

    indent_level: usize,
}

pub type LexResult = Result<TokenInfo, self::Error>;

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        // If we have any queued tokens (layout), return them
        if let Some(queued_token) = self.queue.pop_front() {
            return Some(Ok(queued_token));
        }

        // Skip spaces; record if we encountered a newline.
        self.has_newline = false;
        self.whitespace_start = self.pos;
        let mut line_start: Option<usize> = None;
        loop {
            if self.eof() {
                // drain layout stack
                if self.layout_stack.pop().is_some() {
                    self.token_start = self.pos;
                    return Some(self.make_token(Token::LayoutEnd));
                }
                return None;
            }
            let c = self.peek();
            if c == '\n' {
                self.line_start = self.pos;
                line_start = Some(self.pos);
            }
            self.has_newline = self.has_newline || c == '\n';
            match c {
                // Single-line comment
                '-' if self.can_peek2() && self.peek2() == '-' => {
                    while !self.eof() && self.peek() != '\n' {
                        self.next_char();
                    }
                }
                c if !c.is_whitespace() => {
                    break;
                }
                _ => {}
            }
            self.next_char();
        }
        if let Some(line_start) = line_start {
            self.indent_level = self.pos - line_start;
        }

        let result = self.lex_token();
        let (prev_token, next_token) = if let Ok(next_token) = result {
            let mut prev_token = Some(next_token.clone());
            std::mem::swap(&mut self.last_token, &mut prev_token);
            (prev_token, next_token)
        } else {
            return Some(result);
        };
        while let Some(&indent_level) = self.layout_stack.last() {
            #[allow(clippy::comparison_chain)]
            if next_token.column < indent_level {
                self.layout_stack.pop();
                self.enqueue(self.make_token_info(Token::LayoutEnd));
            } else if next_token.column == indent_level {
                self.enqueue(self.make_token_info(Token::LayoutSep));
                break;
            } else {
                break;
            }
        }
        if let Some(prev_token) = &prev_token {
            #[allow(clippy::single_match)]
            match &prev_token.token {
                Token::Do | Token::Let | Token::Where
                    if next_token.column > prev_token.indent_level =>
                {
                    self.layout_stack.push(next_token.column);
                    self.enqueue(self.make_token_info(Token::LayoutStart));
                }
                _ => {}
            }
        }
        // We may have queued some token(s) above, if so queue the current one and return
        // the one from queue
        if let Some(queued_token) = self.queue.pop_front() {
            self.queue.push_back(next_token);
            return Some(Ok(queued_token));
        }
        Some(Ok(next_token))
    }
}

impl<'a> Lexer<'a> {
    fn enqueue(&mut self, token: TokenInfo) {
        self.queue.push_back(token);
    }

    fn lex_token(&mut self) -> LexResult {
        let c = self.peek();
        self.token_start = self.pos;
        self.next_char();

        // Determine token type
        match c {
            c if is_ident_start(c) => {
                while !self.eof() && is_ident_char(self.peek()) {
                    self.next_char();
                }
                self.make_token(ident_to_token(&self.input[self.token_start..self.pos]))
            }
            c if is_digit(c) => {
                let mut value = digit_value(c);
                while !self.eof() && is_integer_literal_char(self.peek()) {
                    if is_digit(self.peek()) {
                        value = value * 10 + digit_value(self.peek());
                    }
                    self.next_char();
                }
                self.make_token(Token::IntegerLiteral(value))
            }
            '"' => {
                let s = self.parse_string_literal()?;
                self.make_token(Token::StringLiteral(s))
            }
            '\'' => {
                let c = self.parse_possibly_escaped_char()?;
                if self.peek() != '\'' {
                    return Err(Error(format!(
                        "Invalid end of character literal: {}",
                        self.peek()
                    )));
                }
                self.next_char();
                self.make_token(Token::CharLiteral(c))
            }
            ':' => match self.peek() {
                '"' => {
                    self.next_char();
                    let s = self.parse_string_literal()?;
                    self.make_token(Token::Symbol(s))
                }
                _ => {
                    while !self.eof() && is_symbol_char(self.peek()) {
                        self.next_char();
                    }
                    self.make_token(Token::Symbol(
                        String::from_utf8(self.input[(self.token_start + 1)..self.pos].to_vec())
                            .unwrap(),
                    ))
                }
            },
            '(' => self.make_token(Token::LParen),
            ')' => self.make_token(Token::RParen),
            '=' => self.make_token(Token::Equal),
            '|' => self.make_token(Token::Pipe),
            '@' => self.make_token(Token::At),
            ',' => self.make_token(Token::Comma),
            ';' => self.make_token(Token::Semicolon),
            '.' => self.make_token(Token::Dot),
            '-' => self.make_token(Token::Minus),
            _ => Err(Error(format!("Unknown character: {}", c))),
        }
    }
    fn eof(&self) -> bool {
        self.pos >= self.input.len()
    }
    fn peek(&self) -> char {
        // FIXME: we should handle utf8 (using char_indices iterator)
        self.input[self.pos] as char
    }
    fn can_peek2(&self) -> bool {
        self.pos + 2 < self.input.len()
    }
    fn peek2(&self) -> char {
        self.input[self.pos + 1] as char
    }
    fn next_char(&mut self) {
        self.pos += 1;
    }

    fn make_token(&self, token: Token) -> LexResult {
        Ok(self.make_token_info(token))
    }

    fn make_token_info(&self, token: Token) -> TokenInfo {
        TokenInfo {
            token,
            whitespace_start: self.whitespace_start,
            start: self.token_start,
            end: self.pos,
            indent_level: self.indent_level,
            newline_before: self.has_newline,
            column: self.token_start - self.line_start,
        }
    }

    fn parse_string_literal(&mut self) -> Result<String, Error> {
        let mut content = String::new();
        loop {
            if self.eof() {
                return Err(Error("Unterminated string literal".to_string()));
            }
            if self.peek() == '"' {
                self.next_char();
                break;
            }
            content.push(self.parse_possibly_escaped_char()?);
        }
        Ok(content)
    }

    fn parse_possibly_escaped_char(&mut self) -> Result<char, Error> {
        if self.eof() {
            return Err(Error("Unterminated string literal".to_string()));
        }
        let c = match self.peek() {
            '\\' => {
                self.next_char();
                if self.eof() {
                    return Err(Error("Unterminated string literal".to_string()));
                }
                match self.peek() {
                    'n' => '\n',
                    't' => '\t',
                    c => c,
                }
            }
            c => c,
        };
        self.next_char();
        Ok(c)
    }
}

fn ident_to_token(ident: &[u8]) -> Token {
    match ident {
        b"nil" => Token::Nil,
        b"self" => Token::Self_,
        b"if" => Token::If,
        b"then" => Token::Then,
        b"else" => Token::Else,
        b"do" => Token::Do,
        b"end" => Token::End,
        b"while" => Token::While,
        b"let" => Token::Let,
        b"in" => Token::In,
        b"where" => Token::Where,
        _ => Token::Identifier(String::from_utf8(ident.to_vec()).unwrap()),
    }
}

fn digit_value(c: char) -> usize {
    c as usize - '0' as usize
}

fn is_ident_start(c: char) -> bool {
    ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || c == '_'
}

fn is_ident_char(c: char) -> bool {
    is_ident_start(c) || is_digit(c) || c == '?' || c == '!'
}

fn is_symbol_char(c: char) -> bool {
    is_ident_char(c) || c == '='
}

fn is_digit(c: char) -> bool {
    ('0'..='9').contains(&c)
}

fn is_integer_literal_char(c: char) -> bool {
    is_digit(c) || c == '_'
}

#[cfg(test)]
mod tests {
    use std::{fs::File, io::Read, path::PathBuf, str::FromStr};

    use indoc::indoc;
    use insta::{assert_debug_snapshot, assert_snapshot};
    use test_generator::test_resources;

    use super::{Error, Token, TokenInfo};

    fn try_collect<T, E>(iter: impl Iterator<Item = Result<T, E>>) -> Result<Vec<T>, E> {
        let mut result = vec![];
        for item in iter {
            result.push(item?);
        }
        Ok(result)
    }

    fn test_lex(input: &str, expected_result: Result<Vec<Token>, Error>) {
        let result = lex(input);
        if result != expected_result {
            panic!("Test failed.\n            input: {:?}\n  expected result: {:?}\n    actual result: {:?}\n",
                   input, expected_result, result);
        }
    }

    fn lex(input: &str) -> Result<Vec<Token>, Error> {
        try_collect(super::lex(input).map(|v| v.map(|v| v.token)))
    }

    #[test]
    fn test_empty() {
        test_lex("", Ok(vec![]));
        test_lex("  \n  ", Ok(vec![]));
    }

    #[test]
    fn test_identifier() {
        test_lex(
            "asdzASDZ_09",
            Ok(vec![Token::Identifier("asdzASDZ_09".to_string())]),
        );
        test_lex(
            "exists?",
            Ok(vec![Token::Identifier("exists?".to_string())]),
        );
        test_lex("send!", Ok(vec![Token::Identifier("send!".to_string())]));
    }

    #[test]
    fn test_symbol() {
        test_lex(
            ":asdzASDZ_09=",
            Ok(vec![Token::Symbol("asdzASDZ_09=".to_string())]),
        );
        test_lex(":then", Ok(vec![Token::Symbol("then".to_string())]));
        test_lex(r#"  :"foo"  "#, Ok(vec![Token::Symbol("foo".to_string())]));
    }

    #[test]
    fn test_integer_literal() {
        test_lex("123", Ok(vec![Token::IntegerLiteral(123)]));
        test_lex("35_000_000", Ok(vec![Token::IntegerLiteral(35_000_000)]));
    }

    #[test]
    fn test_string_literal() {
        test_lex(r#" "" "#, Ok(vec![Token::StringLiteral("".to_string())]));
        test_lex(
            r#" "Hello" "#,
            Ok(vec![Token::StringLiteral("Hello".to_string())]),
        );
        test_lex(
            r#" "a\n\"\\" "#,
            Ok(vec![Token::StringLiteral("a\n\"\\".to_string())]),
        );
    }

    #[test]
    fn test_string_literal_errors() {
        test_lex(
            r#"""#,
            Err(Error("Unterminated string literal".to_string())),
        );
        test_lex(
            r#""\"#,
            Err(Error("Unterminated string literal".to_string())),
        );
    }

    #[test]
    fn test_keywords() {
        test_lex("then", Ok(vec![Token::Then]));
        test_lex("do", Ok(vec![Token::Do]));
        test_lex("self", Ok(vec![Token::Self_]));
    }

    #[test]
    fn test_operators() {
        test_lex(
            "( ) = | @ , ; . -",
            Ok(vec![
                Token::LParen,
                Token::RParen,
                Token::Equal,
                Token::Pipe,
                Token::At,
                Token::Comma,
                Token::Semicolon,
                Token::Dot,
                Token::Minus,
            ]),
        );
    }

    #[test]
    fn test_comment() {
        test_lex(
            "
            if -- This is a comment
            1
            ",
            Ok(vec![Token::If, Token::IntegerLiteral(1)]),
        );
        test_lex("if--comment", Ok(vec![Token::If]));
    }

    #[test]
    fn test_comment_start_at_eof() {
        test_lex("-", Ok(vec![Token::Minus]));
    }

    #[test_resources("purescript/tests/purs/layout/*.purs")]
    fn layout_example(input_filename: &str) {
        let output_filename = PathBuf::from_str(input_filename)
            .unwrap()
            .with_extension("out");
        println!("{:?}", output_filename);
        let mut input = String::new();
        File::open(input_filename)
            .unwrap()
            .read_to_string(&mut input)
            .unwrap();
        let mut expected_output = String::new();
        File::open(output_filename)
            .unwrap()
            .read_to_string(&mut expected_output)
            .unwrap();
        let output = print_layout(&input);
        pretty_assertions::assert_eq!(output, expected_output);
    }

    fn print_layout(input: &str) -> String {
        let result = try_collect(super::lex(input)).unwrap();
        result
            .iter()
            .flat_map(|t| print_token(input, t).chars())
            .collect::<String>()
            + "\n<eof>"
    }

    fn print_token<'a>(input: &'a str, t: &TokenInfo) -> &'a str {
        match &t.token {
            Token::LayoutStart => "{",
            Token::LayoutSep => ";",
            Token::LayoutEnd => "}",
            _ => &input[t.whitespace_start..t.end],
        }
    }

    #[test]
    fn test_layout_do_1() {
        assert_snapshot!(print_layout(indoc!("
            test = do
                foo
                bar
        ")), @r###"
        test = do{
            foo;
            bar}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_do_2() {
        assert_snapshot!(print_layout(indoc!("
            test = do foo
                      bar
        ")), @r###"
        test = do{ foo
                  bar}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_do_3() {
        assert_snapshot!(print_layout(indoc!("
            test = do
                foo bar
                baz
        ")), @r###"
        test = do{
            foo bar;
            baz}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_do_nested() {
        assert_snapshot!(print_layout(indoc!("
            test = do
                do
                  foo
                  baz
                bar
        ")), @r###"
        test = do{
            do{
              foo;
              baz};
            bar}
        <eof>
        "###);
    }

    #[test]
    fn test_character_literal() {
        assert_debug_snapshot!(lex("'a'"), @r###"
        Ok(
            [
                CharLiteral(
                    'a',
                ),
            ],
        )
        "###);
    }
}
