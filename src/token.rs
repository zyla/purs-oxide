#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    IntegerLiteral(usize),
    StringLiteral(String),
    Identifier(String),
    Symbol(String),

    // Layout
    LayoutStart,
    LayoutSep,
    LayoutEnd,

    // Operators
    LParen,
    RParen,
    Equal,
    Pipe,
    Comma,
    Semicolon,
    Dot,
    Minus,

    // Not technically operators, but parsed the same way
    At,

    // Keywords
    Nil,
    Self_,
    If,
    Then,
    Else,
    Do,
    End,
    While,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TokenInfo {
    pub token: Token,
    /** Position where the whitespace before the token starts */
    pub whitespace_start: usize,
    /** Position where the token itself starts */
    pub start: usize,
    pub end: usize,
    /** Indentation level, i.e. column number of the first non-whitespace token on the line */
    pub indent_level: usize,
    /** Zero-based column number (offset since line start) */
    pub column: usize,
    pub newline_before: bool,
}
