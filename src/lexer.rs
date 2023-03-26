use std::{collections::VecDeque, fmt::Display};

use crate::token::{Token, TokenInfo};

#[derive(PartialEq, Eq, Debug)]
pub struct Error(pub String);

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub fn lex(input: &str) -> impl Iterator<Item = Spanned<Token, usize, Error>> + '_ {
    make_lexer(input).map(|r| r.map(|t| (t.start, t.token, t.end)))
}

fn make_lexer(input: &str) -> impl Iterator<Item = LexResult> + '_ {
    Lexer {
        input: input.as_bytes(),
        pos: 0,
        token_start: 0,
        whitespace_start: 0,
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
    queue: VecDeque<TokenInfo>,
    last_token: Option<TokenInfo>,
    layout_stack: Vec<LayoutEntry>,
    line_start: usize,

    indent_level: usize,
}

#[derive(Debug)]
struct LayoutEntry {
    indent_level: usize,
    token: Token,

    // For `of` blocks:
    after_patterns: bool,
}

pub type LexResult = Result<TokenInfo, self::Error>;

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        // If we have any queued tokens (layout), return them
        if let Some(queued_token) = self.queue.pop_front() {
            return Some(Ok(queued_token));
        }

        // Skip whitespace and comments.
        // Reset indentation state on line breaks.
        self.whitespace_start = self.pos;
        let mut line_start: Option<usize> = None;
        loop {
            if self.eof() {
                // drain layout stack
                while let Some(entry) = self.layout_stack.pop() {
                    if is_indented(&entry.token) {
                        self.token_start = self.pos;
                        return Some(self.make_token(Token::LayoutEnd));
                    }
                }
                return None;
            }
            let c = self.peek();
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
            // Update line position
            // Note: has to be done after single-line comment not to miss its newline
            if !self.eof() && self.peek() == '\n' {
                self.line_start = self.pos + 1;
                line_start = Some(self.line_start);
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

        //dbg!(&next_token.token, &self.layout_stack);

        let mut pushed_backtick = false;
        while let Some(entry) = self.layout_stack.last() {
            // dedent ends indented blocks
            if next_token.column < entry.indent_level && is_indented(&entry.token) {
                let token = self.make_token_info(Token::LayoutEnd);
                self.enqueue(token);
                self.layout_stack.pop();
                continue;
            }

            // `in` ends `let` and `ado` blocks
            if let (Token::Let | Token::Ado, Token::In) = (&entry.token, &next_token.token) {
                let token = self.make_token_info(Token::LayoutEnd);
                self.enqueue(token);
                self.layout_stack.pop();
                // Use it only once, otherwise it ends all nested ado/let blocks
                break;
            }

            // End paren pairs
            if matches!(
                (&entry.token, &next_token.token),
                (Token::LeftParen, Token::RightParen)
                    | (Token::LeftBrace, Token::RightBrace)
                    | (Token::LeftBracket, Token::RightBracket)
                    | (Token::Case, Token::Of)
                    | (Token::If, Token::Then)
                    | (Token::Then, Token::Else)
                    | (Token::Backslash, Token::Arrow)
                    | (Token::Pipe, Token::Arrow | Token::Equal)
            ) {
                self.layout_stack.pop();

                if let Some(entry) = self.layout_stack.last() {
                    // Keep track of arrows in case patterns
                    if matches!((&entry.token, &next_token.token), (Token::Of, Token::Arrow))
                        && !entry.after_patterns
                    {
                        self.layout_stack
                            .last_mut()
                            .expect("layout stack should be non-empty")
                            .after_patterns = true;
                    }
                }
                // Use it only once, otherwise it ends all nested pairs
                break;
            }

            // Keep track of arrows in case patterns
            if matches!((&entry.token, &next_token.token), (Token::Of, Token::Arrow))
                && !entry.after_patterns
            {
                self.layout_stack
                    .last_mut()
                    .expect("layout stack should be non-empty")
                    .after_patterns = true;
                // Use it only once, otherwise it ends all nested pairs
                break;
            }

            // where, of, else and commas end `do` or `case` blocks
            if matches!(
                (&entry.token, &next_token.token),
                (
                    Token::Do,
                    Token::Where | Token::Of | Token::Comma | Token::Else | Token::Arrow
                )
            ) || (matches!((&entry.token, &next_token.token), (Token::Of, Token::Comma))
                && entry.after_patterns)
            {
                let token = self.make_token_info(Token::LayoutEnd);
                self.enqueue(token);
                self.layout_stack.pop();
                // Recursively
                continue;
            }

            // Operator or where in a do or case block at the same indent level ends the block
            if next_token.column == entry.indent_level && is_indented(&entry.token) {
                if let (
                    Token::Do | Token::Of,
                    Token::Operator(_) | Token::Backtick | Token::Where,
                ) = (&entry.token, &next_token.token)
                {
                    let token = self.make_token_info(Token::LayoutEnd);
                    self.enqueue(token);
                    self.layout_stack.pop();
                    continue;
                }
                if has_separators(&entry.token)
                    &&
                        // exclude `else` in instance chains, which should appear at column 0
                        !matches!(
                        next_token,
                        TokenInfo {
                            column: 0,
                            token: Token::Else,
                            ..
                        }
                    )
                {
                    if entry.token == Token::Of {
                        self.layout_stack
                            .last_mut()
                            .expect("layout stack should be non-empty")
                            .after_patterns = false;
                    }

                    let token = self.make_token_info(Token::LayoutSep);
                    self.enqueue(token);
                }
                break;
            }

            // Backtick, if there is a backtick layout stack entry above,
            // ends all indented blocks below it. Otherwise starts a new block.
            if !pushed_backtick && next_token.token == Token::Backtick {
                match find_parent_backtick(&self.layout_stack) {
                    Some(num_blocks_to_drop) => {
                        for _ in 0..num_blocks_to_drop {
                            self.layout_stack.pop();
                            let token = self.make_token_info(Token::LayoutEnd);
                            self.enqueue(token);
                        }
                        // Pop the backtick entry
                        self.layout_stack.pop();
                        break;
                    }
                    None => {
                        self.layout_stack.push(LayoutEntry {
                            indent_level: next_token.column,
                            token: Token::Backtick,
                            after_patterns: false,
                        });
                        pushed_backtick = true;
                        continue;
                    }
                }
            }

            // If no recursive rule triggered via `continue`, stop.
            break;
        }

        // Starting a new layout block
        if let Some(prev_token) = &prev_token {
            match &prev_token.token {
                // Where doesn't need additional indentation
                Token::Where if next_token.column >= prev_token.indent_level => {
                    self.layout_stack.push(LayoutEntry {
                        indent_level: next_token.column,
                        token: prev_token.token.clone(),
                        after_patterns: false,
                    });
                    let token = self.make_token_info(Token::LayoutStart);
                    self.enqueue(token);
                }
                Token::Do | Token::Let | Token::Of | Token::Ado
                    if next_token.column > prev_token.indent_level =>
                {
                    self.layout_stack.push(LayoutEntry {
                        indent_level: next_token.column,
                        token: prev_token.token.clone(),
                        after_patterns: false,
                    });
                    let token = self.make_token_info(Token::LayoutStart);
                    self.enqueue(token);

                    // Annoying edge case: empty let/ado blocks
                    if let (Token::Let | Token::Ado, Token::In) =
                        (&prev_token.token, &next_token.token)
                    {
                        self.layout_stack.pop();
                        let token = self.make_token_info(Token::LayoutEnd);
                        self.enqueue(token);
                    }
                }
                // Various parens introduce a stack entry, but not layout tokens.
                // Also: case..of, if..then, then..else, \..->
                Token::LeftBrace
                | Token::LeftParen
                | Token::LeftBracket
                | Token::Case
                | Token::If
                | Token::Then
                | Token::Backslash => {
                    self.layout_stack.push(LayoutEntry {
                        indent_level: next_token.column,
                        token: prev_token.token.clone(),
                        after_patterns: false,
                    });
                }
                Token::Pipe
                    if self.layout_stack.last().as_ref().map(|x| &x.token) == Some(&Token::Of) =>
                {
                    self.layout_stack.push(LayoutEntry {
                        indent_level: next_token.column,
                        token: prev_token.token.clone(),
                        after_patterns: false,
                    });
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

fn find_parent_backtick(layout_stack: &[LayoutEntry]) -> Option<usize> {
    for (n, entry) in layout_stack.iter().rev().enumerate() {
        if entry.token == Token::Backtick {
            return Some(n);
        }
        if !is_indented(&entry.token) {
            return None;
        }
    }
    None
}

fn is_indented(token: &Token) -> bool {
    matches!(
        token,
        Token::Where | Token::Do | Token::Let | Token::Of | Token::Ado | Token::Else
    )
}

// `else` is indented, but has no separators, only a single expression inside.
fn has_separators(token: &Token) -> bool {
    matches!(
        token,
        Token::Where | Token::Do | Token::Let | Token::Of | Token::Ado
    )
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
                let mut current_segment_is_upper = c.is_uppercase();
                while !self.eof() {
                    if is_ident_char(self.peek()) {
                        self.next_char();
                        continue;
                    } else if current_segment_is_upper && self.peek() == '.' {
                        self.next_char();
                        current_segment_is_upper = !self.eof() && self.peek().is_uppercase();
                        continue;
                    } else {
                        break;
                    }
                }
                self.make_token(ident_to_token(
                    &self.input[self.token_start..self.pos],
                    current_segment_is_upper,
                ))
            }
            c if is_digit(c) => {
                let mut value = digit_value(c);
                while !self.eof() && is_integer_literal_char(self.peek()) {
                    if is_digit(self.peek()) {
                        value = value * 10 + digit_value(self.peek());
                    }
                    self.next_char();
                }
                self.make_token(Token::IntegerLiteral(value as i32)) // FIXME: handle overflow
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
            '(' => self.make_token(Token::LeftParen),
            ')' => self.make_token(Token::RightParen),
            '{' => self.make_token(Token::LeftBrace),
            '}' => self.make_token(Token::RightBrace),
            '[' => self.make_token(Token::LeftBracket),
            ']' => self.make_token(Token::RightBracket),
            '`' => self.make_token(Token::Backtick),
            ',' => self.make_token(Token::Comma),
            ';' => self.make_token(Token::Semicolon),
            c if is_operator_char(c) => {
                while !self.eof() && is_operator_char(self.peek()) {
                    self.next_char();
                }
                self.make_token(operator_to_token(&self.input[self.token_start..self.pos]))
            }

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

    fn make_token(&mut self, token: Token) -> LexResult {
        Ok(self.make_token_info(token))
    }

    fn make_token_info(&mut self, token: Token) -> TokenInfo {
        let token_end = self.pos;
        while !self.eof() && self.peek() == ' ' {
            self.next_char();
        }
        let trailing_space_end = self.pos;
        TokenInfo {
            token,
            leading_space_start: self.whitespace_start,
            start: self.token_start,
            end: token_end,
            trailing_space_end,
            indent_level: self.indent_level,
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

fn ident_to_token(ident: &[u8], is_upper: bool) -> Token {
    match ident {
        b"if" => Token::If,
        b"then" => Token::Then,
        b"else" => Token::Else,
        b"ado" => Token::Ado,
        b"do" => Token::Do,
        b"case" => Token::Case,
        b"of" => Token::Of,
        b"let" => Token::Let,
        b"in" => Token::In,
        b"where" => Token::Where,
        b"instance" => Token::Instance,
        b"module" => Token::Module,
        b"import" => Token::Import,
        b"forall" => Token::Forall,
        b"true" => Token::True,
        b"false" => Token::False,
        b"class" => Token::Class,
        b"type" => Token::Type,
        b"_" => Token::Wildcard,
        _ => {
            let str = String::from_utf8(ident.to_vec()).unwrap();
            if is_upper {
                if str.contains('.') {
                    Token::QualifiedUpperIdentifier(str)
                } else {
                    Token::UpperIdentifier(str)
                }
            } else if str.contains('.') {
                Token::QualifiedLowerIdentifier(str)
            } else {
                Token::LowerIdentifier(str)
            }
        }
    }
}

fn operator_to_token(s: &[u8]) -> Token {
    match s {
        b"=" => Token::Equal,
        b"|" => Token::Pipe,
        b";" => Token::Semicolon,
        b":" => Token::Colon,
        b"." => Token::Dot,
        b"\\" => Token::Backslash,
        b"->" => Token::Arrow,
        b"=>" => Token::FatArrow,
        b"::" => Token::TypeOf,
        b"<-" => Token::Bind,
        b".." => Token::DotDot,
        _ => Token::Operator(String::from_utf8(s.into()).unwrap()),
    }
}

fn digit_value(c: char) -> usize {
    c as usize - '0' as usize
}

fn is_ident_start(c: char) -> bool {
    ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || c == '_'
}

fn is_ident_char(c: char) -> bool {
    is_ident_start(c) || is_digit(c) || c == '\''
}

fn is_operator_char(c: char) -> bool {
    ":!#$%&*+./<=>?@\\^|-~".contains(c)
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
        try_collect(super::make_lexer(input).map(|v| v.map(|v| v.token)))
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
            Ok(vec![Token::LowerIdentifier("asdzASDZ_09".to_string())]),
        );
        test_lex("dont", Ok(vec![Token::LowerIdentifier("dont".to_string())]));
        test_lex(
            "IntType'",
            Ok(vec![Token::UpperIdentifier("IntType'".to_string())]),
        );
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
        test_lex("ado", Ok(vec![Token::Ado]));
    }

    #[test]
    fn test_operators() {
        assert_debug_snapshot!(
        lex(            "()  {} = , ; . + - <$> .= := =. ^|. (<$>)"),
        @r###"
        Ok(
            [
                LeftParen,
                RightParen,
                LeftBrace,
                RightBrace,
                Equal,
                Comma,
                Semicolon,
                Dot,
                Operator(
                    "+",
                ),
                Operator(
                    "-",
                ),
                Operator(
                    "<$>",
                ),
                Operator(
                    ".=",
                ),
                Operator(
                    ":=",
                ),
                Operator(
                    "=.",
                ),
                Operator(
                    "^|.",
                ),
                LeftParen,
                Operator(
                    "<$>",
                ),
                RightParen,
            ],
        )
        "###
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
        assert_debug_snapshot!(
        lex("-"),
        @r###"
        Ok(
            [
                Operator(
                    "-",
                ),
            ],
        )
        "###
                );
    }

    #[test_resources("purescript/tests/purs/layout/*.purs")]
    fn layout_example(input_filename: &str) {
        const IGNORES: &[&str] = &[];
        if IGNORES.iter().any(|i| input_filename.contains(i))
            && !std::env::var("INCLUDE_IGNORED")
                .map(|x| x == "1")
                .unwrap_or(false)
        {
            // Test ignored.
            // Unfortunately there seems to be no way to tell that to the test runner,
            // other than through #[ignore]. But `test_generator` doesn't support that.
            return;
        }

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
        let result = try_collect(super::make_lexer(input)).unwrap();
        // We need to preserve trailing whitespace.
        // Since layout tokens have broken positions (which arguably we could fix, but it's not
        // done currently), we find the last "real" token and use that.
        let last_real_token_end = result
            .iter()
            .rev()
            .find(|t| {
                !matches!(
                    t.token,
                    Token::LayoutStart | Token::LayoutEnd | Token::LayoutSep
                )
            })
            .unwrap()
            .trailing_space_end;
        result
            .iter()
            .flat_map(|t| print_token(input, t).chars())
            .collect::<String>()
            + &input[last_real_token_end..]
            + "<eof>"
    }

    fn print_token<'a>(input: &'a str, t: &TokenInfo) -> &'a str {
        match &t.token {
            Token::LayoutStart => "{",
            Token::LayoutSep => ";",
            Token::LayoutEnd => "}",
            _ => &input[t.leading_space_start..t.trailing_space_end],
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
        test = do {foo;
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
    fn test_layout_do_where() {
        assert_snapshot!(print_layout(indoc!("
            test = do foo bar where x = 1
        ")), @r###"
        test = do {foo bar }where {x = 1}
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
    fn test_layout_module_item_sep() {
        assert_snapshot!(print_layout(indoc!("
            module Foo where
            x = 1
            y = 2
        ")), @r###"
        module Foo where{
        x = 1;
        y = 2}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_module_item_sep_2() {
        assert_snapshot!(print_layout(indoc!("
            module Foo where
            x =
                foo
                bar
            y = 2
        ")), @r###"
        module Foo where{
        x =
            foo
            bar;
        y = 2}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_module_item_sep_3() {
        assert_snapshot!(print_layout(indoc!("
            module Foo where
            x = {
                foo
            }
            y = 2
        ")), @r###"
        module Foo where{
        x = {
            foo
        };
        y = 2}
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

    #[test]
    fn test_layout_data_1() {
        assert_snapshot!(print_layout(indoc!("
            module Foo where
            data Foo =
                Foo
                | Bar
            y = 2
        ")), @r###"
        module Foo where{
        data Foo =
            Foo
            | Bar;
        y = 2}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_data_2() {
        assert_snapshot!(print_layout(indoc!("
            module Foo where
            data Foo
                = Foo
                | Bar
            y = 2
        ")), @r###"
        module Foo where{
        data Foo
            = Foo
            | Bar;
        y = 2}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_item_comment() {
        assert_snapshot!(print_layout(indoc!("
            module Foo where
            -- test
            y = foo
                bar
        ")), @r###"
        module Foo where{
        -- test
        y = foo
            bar}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_shebang() {
        assert_snapshot!(print_layout(indoc!("
            #! shebang
            module Foo where
            y = 1
        ")), @r###"
        #! shebang
        module Foo where{
        y = 1}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_case_do_1() {
        assert_snapshot!(print_layout(indoc!("
            y = case do foo of x -> x
        ")), @r###"
        y = case do {foo }of {x -> x}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_case_do_2() {
        assert_snapshot!(print_layout(indoc!("
            y = case do foo, bar of x, y -> x
        ")), @r###"
        y = case do {foo}, bar of {x, y -> x}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_case_do_3() {
        assert_snapshot!(print_layout(indoc!("
            y = do case foo, bar of x, y -> x
        ")), @r###"
        y = do {case foo, bar of {x, y -> x}}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_if() {
        assert_snapshot!(print_layout(indoc!("
            y = if foo then
                    bar
                else
                    baz
        ")), @r###"
        y = if foo then
                bar
            else
                baz
        <eof>
        "###);
    }

    #[test]
    fn test_layout_do_if() {
        assert_snapshot!(print_layout(indoc!("
            y = do
                if foo then
                    bar
                else
                    baz
                q
        ")), @r###"
        y = do{
            if foo then
                bar
            else
                baz;
            q}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_do_if_2() {
        assert_snapshot!(print_layout(indoc!("
            y = do
                if
                    foo
                    bar
                then
                    bar
                    baz
                else
                    baz
                    qux
                q
        ")), @r###"
        y = do{
            if
                foo
                bar
            then
                bar
                baz
            else
                baz
                qux;
            q}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_do_if_3() {
        assert_snapshot!(print_layout(indoc!("
            y = do
                if foo then do
                    bar
                    baz
                    else do
                        baz
                        qux
                q
        ")), @r###"
        y = do{
            if foo then do{
                bar;
                baz}
                else do{
                    baz;
                    qux};
            q}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_list() {
        assert_snapshot!(print_layout(indoc!("
            module Foo where
            x = [ 1, 2 ]
            y = 1
        ")), @r###"
        module Foo where{
        x = [ 1, 2 ];
        y = 1}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_backtick() {
        assert_snapshot!(print_layout(indoc!("
            module Foo where
            x = 1 `add` 2
            y = 1
            z = 1
        ")), @r###"
        module Foo where{
        x = 1 `add` 2;
        y = 1;
        z = 1}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_case_backtick() {
        assert_snapshot!(print_layout(indoc!(r#"
            module Foo where
            test = a `case _ of x | unit # \_ -> true, true -> const` b
            foo
        "#)), @r###"
        module Foo where{
        test = a `case _ of {x | unit # \_ -> true, true -> const}` b;
        foo}
        <eof>
        "###);
    }

    #[test]
    fn test_qualified_ident_1() {
        assert_debug_snapshot!(
        lex("Foo.Bar"),
        @r###"
        Ok(
            [
                QualifiedUpperIdentifier(
                    "Foo.Bar",
                ),
            ],
        )
        "###);
    }

    #[test]
    fn test_qualified_ident_2() {
        assert_debug_snapshot!(
        lex("Foo.Bar.t"),
        @r###"
        Ok(
            [
                QualifiedLowerIdentifier(
                    "Foo.Bar.t",
                ),
            ],
        )
        "###);
    }

    #[test]
    fn test_qualified_ident_3() {
        assert_debug_snapshot!(
        lex("Foo.Bar.t.baz"),
        @r###"
        Ok(
            [
                QualifiedLowerIdentifier(
                    "Foo.Bar.t",
                ),
                Dot,
                LowerIdentifier(
                    "baz",
                ),
            ],
        )
        "###);
    }

    #[test]
    fn test_qualified_ident_4() {
        assert_debug_snapshot!(
        lex("Foo.Bar.t.\"Baz\""),
        @r###"
        Ok(
            [
                QualifiedLowerIdentifier(
                    "Foo.Bar.t",
                ),
                Dot,
                StringLiteral(
                    "Baz",
                ),
            ],
        )
        "###);
    }

    #[test]
    fn test_qualified_ident_5() {
        assert_debug_snapshot!(
        lex("foo.bar"),
        @r###"
        Ok(
            [
                LowerIdentifier(
                    "foo",
                ),
                Dot,
                LowerIdentifier(
                    "bar",
                ),
            ],
        )
        "###);
    }

    #[test]
    fn test_qualified_ident_6() {
        assert_debug_snapshot!(
        lex("Data.Maybe.fromJust"),
        @r###"
        Ok(
            [
                QualifiedLowerIdentifier(
                    "Data.Maybe.fromJust",
                ),
            ],
        )
        "###);
    }

    //
}
