use crate::string::{PSChar, PSString};
use log::trace;
use std::iter::Peekable;
use std::{collections::VecDeque, fmt::Display, str::CharIndices};
use unicode_general_category::{get_general_category, GeneralCategory::*};

pub use crate::token::{Token, TokenInfo};

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct LexerError(pub String);

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct Loc {
    pub start: usize,
    pub end: usize,
}

impl Loc {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Error {
    pub loc: Loc,
    pub kind: ErrorKind,
}

impl Error {
    pub fn new(start: usize, end: usize, kind: ErrorKind) -> Self {
        Self {
            loc: Loc::new(start, end),
            kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    InvalidClassHead,
    InvalidInstanceHead,
    InvalidFloatingPointNumber,
    NonUsvChar,
    Unknown(String),
    Error(LexerError),
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub fn lex(input: &str) -> impl Iterator<Item = Spanned<Token, usize, Error>> + '_ {
    make_lexer(input).map(|r| r.map(|t| (t.start, t.token, t.end)))
}

fn make_lexer(input: &str) -> impl Iterator<Item = LexResult> + '_ {
    let mut chars = input.char_indices().peekable();
    let first = chars.next();
    Lexer {
        input,
        chars,
        pos: first.map(|x| x.0).unwrap_or(0),
        current: first.map(|x| x.1),
        token_start: 0,
        whitespace_start: 0,
        queue: Default::default(),
        last_token: None,
        indent_level: 0,
        line: 0,
        line_start: 0,
        layout_stack: Default::default(),
    }
}

struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<CharIndices<'a>>,
    pos: usize,
    current: Option<char>,
    whitespace_start: usize,
    token_start: usize,
    queue: VecDeque<TokenInfo>,
    last_token: Option<TokenInfo>,
    layout_stack: Vec<LayoutEntry>,
    line: usize,
    line_start: usize,

    indent_level: usize,
}

#[derive(Debug)]
struct LayoutEntry {
    indent_level: usize,
    #[allow(dead_code)]
    line: usize,
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
                trace!("eof, clearing layout stack");
                while let Some(entry) = self.layout_pop() {
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
                // Multi-line comment
                '{' if self.can_peek2() && self.peek2() == '-' => {
                    while !self.eof()
                        && !(self.peek() == '-' && self.can_peek2() && self.peek2() == '}')
                    {
                        self.next_char();
                    }
                    if !self.eof() {
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
            // TODO: update after multiline comments? (probably not necessary, we don't use it for
            // location reporting, only layout)
            if !self.eof() && self.peek() == '\n' {
                self.line_start = self.pos + 1;
                line_start = Some(self.line_start);
                self.line += 1;
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

        trace!("lexed token {:?}", next_token.token);

        // Starting a new paren block
        if let Some(prev_token) = &prev_token {
            match &prev_token.token {
                // Various parens introduce a stack entry, but not layout tokens.
                // Also: case..of, if..then, then..else, \..->
                Token::LeftBrace
                | Token::LeftParen
                | Token::LeftBracket
                | Token::Case
                | Token::If
                | Token::Then
                | Token::Backslash => {
                    self.layout_push(LayoutEntry {
                        line: next_token.line,
                        indent_level: next_token.column,
                        token: prev_token.token.clone(),
                        after_patterns: false,
                    });
                }
                Token::Pipe
                    if self.layout_stack.last().as_ref().map(|x| &x.token) == Some(&Token::Of) =>
                {
                    self.layout_push(LayoutEntry {
                        line: next_token.line,
                        indent_level: next_token.column,
                        token: prev_token.token.clone(),
                        after_patterns: false,
                    });
                }
                _ => {}
            }
        }

        let mut pushed_backtick = false;
        let mut dedented = false;
        while let Some(entry) = self.layout_stack.last() {
            // dedent ends indented blocks
            if next_token.column < entry.indent_level && is_indented(&entry.token) {
                let token = self.make_token_info(Token::LayoutEnd);
                self.enqueue(token);
                trace!("dedent");
                self.layout_pop();
                dedented = true;
                continue;
            }

            // `in` ends `ado` blocks, and `let` if indented more
            if matches!((&entry.token, &next_token.token), (Token::Ado, Token::In))
                || (!dedented
                    && matches!((&entry.token, &next_token.token), (Token::Let, Token::In)))
            {
                let token = self.make_token_info(Token::LayoutEnd);
                self.enqueue(token);
                trace!("in ends");
                self.layout_pop();
                // Use it only once, otherwise it ends all nested ado/let blocks
                break;
            }

            // End paren pairs
            // TODO: redundant? we are also closing parens above (but not all - not if, case etc.)
            if is_matching_paren_pair(&entry.token, &next_token.token) {
                trace!("ending paren pair");
                self.layout_pop();

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

            // where, of, else and commas end `do` blocks;
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
                trace!("ending block via {:?}", &next_token.token);
                self.layout_pop();
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
                    trace!("ending block via dedent");
                    self.layout_pop();
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
                            trace!("ending block via backtick");
                            self.layout_pop();
                            let token = self.make_token_info(Token::LayoutEnd);
                            self.enqueue(token);
                        }
                        // Pop the backtick entry
                        trace!("ending backtick");
                        self.layout_pop();
                        break;
                    }
                    None => {
                        self.layout_push(LayoutEntry {
                            line: next_token.line,
                            indent_level: next_token.column,
                            token: Token::Backtick,
                            after_patterns: false,
                        });
                        pushed_backtick = true;
                        continue;
                    }
                }
            }

            // Similarly, a closing paren ends all layout blocks up to the opening paren.
            if matches!(
                &next_token.token,
                Token::RightParen | Token::RightBrace | Token::RightBracket
            ) && (match &prev_token {
                Some(prev_token) => !is_matching_paren_pair(&prev_token.token, &next_token.token),
                None => false,
            }) {
                match find_parent_matching_paren(&self.layout_stack, &next_token.token) {
                    Some(num_blocks_to_drop) => {
                        for _ in 0..num_blocks_to_drop {
                            trace!("ending block via closing paren");
                            self.layout_pop();
                            let token = self.make_token_info(Token::LayoutEnd);
                            self.enqueue(token);
                        }
                        // Pop the backtick entry
                        trace!("ending paren block");
                        self.layout_pop();
                        break;
                    }
                    None => {}
                }
            }

            // If no recursive rule triggered via `continue`, stop.
            break;
        }

        // Starting a new layout block
        if let Some(prev_token) = &prev_token {
            match &prev_token.token {
                // Where doesn't need additional indentation
                Token::Where => {
                    self.layout_push(LayoutEntry {
                        line: next_token.line,
                        indent_level: next_token.column,
                        token: prev_token.token.clone(),
                        after_patterns: false,
                    });
                    let token = self.make_token_info(Token::LayoutStart);
                    self.enqueue(token);
                }
                Token::Do | Token::Let | Token::Of | Token::Ado
                    if next_token.column >= prev_token.indent_level =>
                {
                    self.layout_push(LayoutEntry {
                        line: next_token.line,
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
                        trace!("ending empty let/ado");
                        self.layout_pop();
                        let token = self.make_token_info(Token::LayoutEnd);
                        self.enqueue(token);
                    }
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
    fn layout_push(&mut self, entry: LayoutEntry) {
        trace!("layout_push({:?})", entry);
        self.layout_stack.push(entry);
    }

    fn layout_pop(&mut self) -> Option<LayoutEntry> {
        let entry = self.layout_stack.pop();
        trace!("layout_pop() -> {:?}", entry);
        entry
    }

    fn loc_error(&self, error: String) -> Error {
        Error {
            loc: Loc {
                start: self.token_start,
                end: self.pos,
            },
            kind: ErrorKind::Error(LexerError(error)),
        }
    }
}

fn is_matching_paren_pair(t1: &Token, t2: &Token) -> bool {
    matches!(
        (t1, t2),
        (Token::LeftParen, Token::RightParen)
            | (Token::LeftBrace, Token::RightBrace)
            | (Token::LeftBracket, Token::RightBracket)
            | (Token::Case, Token::Of)
            | (Token::If, Token::Then)
            | (Token::Then, Token::Else)
            | (Token::Backslash, Token::Arrow)
            | (Token::Pipe, Token::Arrow | Token::Equal)
    )
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

fn find_parent_matching_paren(layout_stack: &[LayoutEntry], end_token: &Token) -> Option<usize> {
    for (n, entry) in layout_stack.iter().rev().enumerate() {
        if is_matching_paren_pair(&entry.token, end_token) {
            return Some(n);
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
                let mut prev = c;
                while !self.eof() {
                    if is_ident_char(self.peek()) {
                        prev = self.peek();
                        self.next_char();
                        continue;
                    } else if current_segment_is_upper && self.peek() == '.' {
                        prev = self.peek();
                        self.next_char();
                        current_segment_is_upper = !self.eof() && self.peek().is_uppercase();
                        continue;
                    } else if prev == '.' && is_operator_char(self.peek()) {
                        let dot = self.pos;
                        // Qualified operator
                        while !self.eof() && is_operator_char(self.peek()) {
                            self.next_char();
                        }
                        return self.make_token(Token::QualifiedOperator((
                            self.input[self.token_start..dot - 1].into(),
                            self.input[dot..self.pos].into(),
                        )));
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
                let is_float = loop {
                    if self.eof() {
                        break false;
                    }
                    if self.peek() == '.' &&
                        // Double dot is actually an operator, don't parse float literal if so
                        !(self.can_peek2() && self.peek2() == '.')
                    {
                        break true;
                    }
                    if !is_integer_literal_char(self.peek()) {
                        break false;
                    }
                    if is_digit(self.peek()) {
                        value = value * 10 + digit_value(self.peek());
                    }
                    self.next_char();
                };
                if is_float {
                    self.next_char();
                    while !self.eof() && is_integer_literal_char(self.peek()) {
                        self.next_char();
                    }
                    self.make_token(Token::FloatLiteral(
                        self.input[self.token_start..self.pos]
                            .chars()
                            .filter(|c| *c != '_')
                            .collect(),
                    ))
                } else {
                    self.make_token(Token::IntegerLiteral(value as u64))
                }
            }
            '"' => {
                let s = self.parse_string_literal()?;
                self.make_token(Token::StringLiteral(s))
            }
            '\'' => {
                let c = self.parse_possibly_escaped_char()?;
                if self.peek() != '\'' {
                    return Err(self
                        .loc_error(format!("Invalid end of character literal: {}", self.peek())));
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

            _ => Err(self.loc_error(format!("Unknown character: {}", c))),
        }
    }
    fn eof(&self) -> bool {
        self.current.is_none()
    }
    fn peek(&self) -> char {
        self.current.expect("eof")
    }
    fn can_peek2(&mut self) -> bool {
        self.chars.peek().is_some()
    }
    fn peek2(&mut self) -> char {
        self.chars.peek().expect("can't peek2").1
    }
    fn next_char(&mut self) {
        let item = self.chars.next();
        self.pos = item.map(|x| x.0).unwrap_or(self.input.len());
        self.current = item.map(|x| x.1);
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
            line: self.line,
            column: self.token_start - self.line_start,
        }
    }

    fn parse_string_literal(&mut self) -> Result<PSString, Error> {
        #[derive(PartialEq, Eq)]
        enum State {
            Beginning,
            Normal,
            Raw,
        }
        use State::*;

        let mut content = vec![];
        let mut state = Beginning;
        let mut num_quotes: u8 = 1;
        loop {
            if self.eof() {
                return Err(self.loc_error("Unterminated string literal".to_string()));
            }
            if self.peek() == '"' {
                self.next_char();
                match state {
                    Beginning => {
                        if !self.eof() && self.peek() == '"' {
                            self.next_char();
                            state = Raw;
                            num_quotes = 0;
                            continue;
                        } else {
                            break;
                        }
                    }
                    Normal => {
                        break;
                    }
                    Raw => {
                        num_quotes += 1;
                        if num_quotes == 3 {
                            break;
                        } else {
                            continue;
                        }
                    }
                }
            }
            if state == Beginning {
                state = Normal;
            }
            if state == Raw {
                for _ in 0..num_quotes {
                    content.push('"' as PSChar);
                }
                num_quotes = 0;
                if self.eof() {
                    return Err(self.loc_error("Unterminated string literal".to_string()));
                }
                content.push(self.peek() as PSChar);
                self.next_char();
            } else {
                content.push(self.parse_possibly_escaped_char()?);
            }
        }
        Ok(PSString(content))
    }

    fn parse_possibly_escaped_char(&mut self) -> Result<PSChar, Error> {
        if self.eof() {
            return Err(self.loc_error("Unterminated string literal".to_string()));
        }
        let c = match self.peek() {
            '\\' => {
                self.next_char();
                if self.eof() {
                    return Err(self.loc_error("Unterminated string literal".to_string()));
                }
                match self.peek() {
                    'n' => '\n' as PSChar,
                    't' => '\t' as PSChar,
                    'r' => '\r' as PSChar,
                    'x' => {
                        self.next_char();
                        let mut num_digits = 0;
                        let mut value = 0;
                        while num_digits < 6 {
                            if self.eof() {
                                return Err(
                                    self.loc_error("Unterminated string literal".to_string())
                                );
                            }
                            let c = self.peek();
                            if !c.is_ascii_hexdigit() {
                                break;
                            }
                            value = value << 4 | (hex_value(c) as u32);
                            num_digits += 1;
                            self.next_char();
                        }
                        if num_digits == 0 {
                            return Err(self.loc_error("Invalid character escape".to_string()));
                        }
                        if value > 0x10ffff {
                            return Err(
                                self.loc_error(format!("character out of range: {:02x}", value))
                            );
                        }
                        return Ok(value);
                    }
                    c => c as PSChar,
                }
            }
            c => c as PSChar,
        };
        self.next_char();
        Ok(c)
    }
}

fn ident_to_token(ident: &str, is_upper: bool) -> Token {
    match ident {
        "if" => Token::If,
        "then" => Token::Then,
        "else" => Token::Else,
        "ado" => Token::Ado,
        "do" => Token::Do,
        "case" => Token::Case,
        "of" => Token::Of,
        "let" => Token::Let,
        "in" => Token::In,
        "where" => Token::Where,
        "instance" => Token::Instance,
        "module" => Token::Module,
        "import" => Token::Import,
        "forall" => Token::Forall,
        "true" => Token::True,
        "false" => Token::False,
        "class" => Token::Class,
        "type" => Token::Type,
        "as" => Token::As,
        "hiding" => Token::Hiding,
        "foreign" => Token::Foreign,
        "derive" => Token::Derive,
        "newtype" => Token::Newtype,
        "data" => Token::Data,
        "infix" => Token::Infix,
        "infixl" => Token::Infixl,
        "infixr" => Token::Infixr,
        "role" => Token::Role,
        "nominal" => Token::Nominal,
        "representational" => Token::Representational,
        "phantom" => Token::Phantom,

        "_" => Token::Wildcard,
        _ => {
            let str = ident.to_string();
            if is_upper {
                match str.rsplit_once('.') {
                    Some((module, name)) => {
                        Token::QualifiedUpperIdentifier((module.into(), name.into()))
                    }
                    None => Token::UpperIdentifier(str),
                }
            } else {
                match str.rsplit_once('.') {
                    Some((module, name)) => {
                        Token::QualifiedLowerIdentifier((module.into(), name.into()))
                    }
                    None => Token::LowerIdentifier(str),
                }
            }
        }
    }
}

fn operator_to_token(s: &str) -> Token {
    match s {
        "=" => Token::Equal,
        "|" => Token::Pipe,
        ";" => Token::Semicolon,
        ":" => Token::Colon,
        "." => Token::Dot,
        "\\" => Token::Backslash,
        "@" => Token::At,
        "-" => Token::Minus,
        "->" => Token::Arrow,
        "→" => Token::Arrow,
        "=>" => Token::FatArrow,
        "⇒" => Token::FatArrow,
        "<=" => Token::LeftFatArrow,
        "⇐" => Token::LeftFatArrow,
        "::" => Token::TypeOf,
        "∷" => Token::TypeOf,
        "<-" => Token::Bind,
        "←" => Token::Bind,
        ".." => Token::DotDot,
        "∀" => Token::Forall,
        _ => Token::Operator(s.into()),
    }
}

fn digit_value(c: char) -> usize {
    c as usize - '0' as usize
}

fn hex_value(c: char) -> u8 {
    if is_digit(c) {
        c as u8 - '0' as u8
    } else {
        c.to_ascii_lowercase() as u8 - 'a' as u8 + 10
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_ident_char(c: char) -> bool {
    is_ident_start(c) || is_digit(c) || c == '\''
}

fn is_operator_char(c: char) -> bool {
    ":!#$%&*+./<=>?@\\^|-~".contains(c)
        || (!c.is_ascii()
            && [MathSymbol, CurrencySymbol, ModifierSymbol, OtherSymbol]
                .contains(&get_general_category(c)))
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

    use super::{Error, ErrorKind, LexerError, Token, TokenInfo};

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

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
        init();
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
        test_lex(r#" "" "#, Ok(vec![Token::StringLiteral("".into())]));
        test_lex(
            r#" "Hello" "#,
            Ok(vec![Token::StringLiteral("Hello".into())]),
        );
        test_lex(
            r#" "a\n\"\\" "#,
            Ok(vec![Token::StringLiteral("a\n\"\\".into())]),
        );
    }

    #[test]
    fn test_raw_string_literal() {
        test_lex(
            r#" """ Hello "world" """1 "#,
            Ok(vec![
                Token::StringLiteral(" Hello \"world\" ".into()),
                Token::IntegerLiteral(1),
            ]),
        );
    }

    #[test]
    fn test_raw_string_literal_2() {
        test_lex(
            r#" " "" " "#,
            Ok(vec![
                Token::StringLiteral(" ".into()),
                Token::StringLiteral(" ".into()),
            ]),
        );
    }

    #[test]
    fn test_raw_string_literal_3() {
        test_lex(
            r#" """.+@.+\..+""" "#,
            Ok(vec![Token::StringLiteral(".+@.+\\..+".into())]),
        );
    }

    #[test]
    fn test_string_literal_errors() {
        test_lex(
            r#"""#,
            Err(Error::new(
                0,
                1,
                ErrorKind::Error(LexerError("Unterminated string literal".to_string())),
            )),
        );
        test_lex(
            r#""\"#,
            Err(Error::new(
                0,
                2,
                ErrorKind::Error(LexerError("Unterminated string literal".to_string())),
            )),
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
                Minus,
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
    fn test_multiline_comment() {
        test_lex(
            "
            if {- This is a comment
            it spans many lines - hello
            -}1
            ",
            Ok(vec![Token::If, Token::IntegerLiteral(1)]),
        );
    }

    #[test]
    fn test_comment_start_at_eof() {
        assert_debug_snapshot!(
        lex("-"),
        @r###"
        Ok(
            [
                Minus,
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
        init();
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
    fn test_layout_do_4() {
        assert_snapshot!(print_layout(indoc!("
        x =
          f $ do
          bar
        ")), @r###"
        x =
          f $ do{
          bar}
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
                    97,
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
    fn test_layout_indented_module_where() {
        assert_snapshot!(print_layout(indoc!("
            module Foo
              where
            x = 1
        ")), @r###"
        module Foo
          where{
        x = 1}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_empty_paren_pairs() {
        assert_snapshot!(print_layout(indoc!("
            module Foo where
            x = {}
            x = []
            x = ()
            y = 1
        ")), @r###"
        module Foo where{
        x = {};
        x = [];
        x = ();
        y = 1}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_case_in_parens() {
        assert_snapshot!(print_layout(indoc!("
            f = (case x of
                   _ -> 2) + 3
        ")), @r###"
        f = (case x of{
               _ -> 2}) + 3
        <eof>
        "###);
    }

    #[test]
    fn test_layout_do_empty_array() {
        assert_snapshot!(print_layout(indoc!("
            do
                []
        ")), @r###"
        do{
            []}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_do_empty_array_2() {
        assert_snapshot!(print_layout(indoc!("
            do
                [[]]
        ")), @r###"
        do{
            [[]]}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_paren_nesting() {
        assert_snapshot!(print_layout(indoc!("
            [ { foo: [], bar: 1 } ]
        ")), @r###"
        [ { foo: [], bar: 1 } ]
        <eof>
        "###);
    }

    #[test]
    fn test_layout_do_list() {
        assert_snapshot!(print_layout(indoc!("
        do
            [[], [1]]
        ")), @r###"
        do{
            [[], [1]]}
        <eof>
        "###);
    }

    #[test]
    #[ignore = "Doesn't work yet"]
    fn test_layout_do_weird_indent() {
        assert_snapshot!(print_layout(indoc!("
        f [
              ] do
          x
        ")), @r###"
        f [
              ] do{
          x}
        <eof>
        "###);
    }

    #[test]
    fn test_layout_let() {
        assert_snapshot!(print_layout(indoc!("
            removes =
              let
                loop =
                  let
                    tl = 2
                  in
                    foo
              in bar
        ")), @r###"
        removes =
          let{
            loop =
              let{
                tl = 2}
              in
                foo}
          in bar
        <eof>
        "###);
    }

    #[test]
    fn test_layout_let_2() {
        assert_snapshot!(print_layout(indoc!("
          let x = 1
              y = 2 in
          foo
        ")), @r###"
        let {x = 1;
            y = 2 }in
        foo
        <eof>
        "###);
    }

    #[test]
    fn test_layout_let_3() {
        assert_snapshot!(print_layout(indoc!("
          test = ado
            baz
            let foo = bar
            in bar
        ")), @r###"
        test = ado{
          baz;
          let {foo = bar}}
          in bar
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
                    (
                        "Foo",
                        "Bar",
                    ),
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
                    (
                        "Foo.Bar",
                        "t",
                    ),
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
                    (
                        "Foo.Bar",
                        "t",
                    ),
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
                    (
                        "Foo.Bar",
                        "t",
                    ),
                ),
                Dot,
                StringLiteral(
                    PSString(
                        [
                            66,
                            97,
                            122,
                        ],
                    ),
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
                    (
                        "Data.Maybe",
                        "fromJust",
                    ),
                ),
            ],
        )
        "###);
    }

    #[test]
    fn test_utf8() {
        assert_debug_snapshot!(
        lex("Zażółć gęślą jaźń"),
        @r###"
        Ok(
            [
                UpperIdentifier(
                    "Zażółć",
                ),
                LowerIdentifier(
                    "gęślą",
                ),
                LowerIdentifier(
                    "jaźń",
                ),
            ],
        )
        "###);
    }

    #[test]
    fn test_utf8_operator() {
        assert_debug_snapshot!(
        lex("∘"),
        @r###"
        Ok(
            [
                Operator(
                    "∘",
                ),
            ],
        )
        "###);
    }

    #[test]
    fn test_unicode_syntax() {
        assert_debug_snapshot!(
        lex("∀ ⇒ ∷ →"),
        @r###"
        Ok(
            [
                Forall,
                FatArrow,
                TypeOf,
                Arrow,
            ],
        )
        "###);
    }

    #[test]
    fn test_unicode_escapes() {
        assert_debug_snapshot!(
        lex("'\\x0' '\\x2713' '\\x02713' '\\x10ffff'"),
        @r###"
        Ok(
            [
                CharLiteral(
                    0,
                ),
                CharLiteral(
                    10003,
                ),
                CharLiteral(
                    10003,
                ),
                CharLiteral(
                    1114111,
                ),
            ],
        )
        "###);
    }

    #[test]
    fn test_non_usv_char() {
        assert_debug_snapshot!(
        lex("'\\xdc00'"),
        @r###"
        Ok(
            [
                CharLiteral(
                    56320,
                ),
            ],
        )
        "###);
    }

    //
}
