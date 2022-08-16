#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    IntegerLiteral(i32),
    StringLiteral(String),
    CharLiteral(char),
    LowerIdentifier(String),
    QualifiedLowerIdentifier(String),
    UpperIdentifier(String),
    QualifiedUpperIdentifier(String),

    // Layout
    LayoutStart,
    LayoutSep,
    LayoutEnd,

    // Operators
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Backtick,
    Equal,
    Pipe,
    Comma,
    Colon,
    Semicolon,
    Dot,
    Backslash,

    Arrow,    // ->
    FatArrow, // =>
    TypeOf,   // ::
    Bind,     // <-

    Operator(String),

    // Keywords
    If,
    Then,
    Else,
    Ado,
    Do,
    Case,
    Of,
    Let,
    In,
    Where,
    Instance,
    Module,
    Import,
    Forall,
    True,
    False,

    // Not exactly a keyword, but a special identifier
    Wildcard,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TokenInfo {
    pub token: Token,
    pub leading_space_start: usize,
    pub trailing_space_end: usize,
    /** Position where the token itself starts */
    pub start: usize,
    pub end: usize,
    /** Indentation level, i.e. column number of the first non-whitespace token on the line */
    pub indent_level: usize,
    /** Zero-based column number (offset since line start) */
    pub column: usize,
}
