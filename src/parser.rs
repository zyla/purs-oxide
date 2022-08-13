use crate::ast::Module;
use crate::ast::Type;
use crate::lexer;
use crate::token::Token;
use lalrpop_util::ErrorRecovery;
use lalrpop_util::ParseError;

lalrpop_mod!(pub parser);

type ParseResult<'a, T> = (
    Vec<ErrorRecovery<usize, Token, &'a str>>,
    Result<T, ParseError<usize, Token, lexer::Error>>,
);

pub fn parse_module(input: &str) -> ParseResult<Module> {
    let mut errors = vec![];
    let lexer = lexer::lex(input);
    let result = parser::ModuleParser::new().parse(&mut errors, lexer);
    (errors, result)
}

pub fn parse_type(input: &str) -> ParseResult<Type> {
    let mut errors = vec![];
    let lexer = lexer::lex(input);
    let result = parser::TypeParser::new().parse(&mut errors, lexer);
    (errors, result)
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use insta::{self, assert_debug_snapshot};

    fn expect_success<T>(output: super::ParseResult<T>) -> T {
        let (errors, result) = output;
        assert_eq!(errors, &[]);
        result.unwrap()
    }

    fn parse_module(input: &str) -> crate::ast::Module {
        expect_success(super::parse_module(input))
    }
    fn parse_type(input: &str) -> crate::ast::Type {
        expect_success(super::parse_type(input))
    }

    #[test]
    fn test_module_header() {
        assert_debug_snapshot!(parse_module(indoc!("
        module Foo where
        ")), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 16,
            },
            Commented(
                [],
                ModuleInner {
                    name: Symbol(
                        "Foo",
                    ),
                    exports: None,
                    declarations: [],
                },
            ),
        )
        "###);
    }

    #[test]
    fn test_simple_value_decl() {
        assert_debug_snapshot!(parse_module(indoc!("
        module Foo where
        x = 1
        ")), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 23,
            },
            Commented(
                [],
                ModuleInner {
                    name: Symbol(
                        "Foo",
                    ),
                    exports: None,
                    declarations: [
                        Located(
                            SourceSpan {
                                start: 17,
                                end: 22,
                            },
                            Commented(
                                [],
                                ValueDeclaration(
                                    ValueDeclaration {
                                        ident: Symbol(
                                            "x",
                                        ),
                                        expr: [
                                            GuardedExpr {
                                                guards: [],
                                                expr: Located(
                                                    SourceSpan {
                                                        start: 21,
                                                        end: 22,
                                                    },
                                                    Literal(
                                                        Integer(
                                                            1,
                                                        ),
                                                    ),
                                                ),
                                            },
                                        ],
                                    },
                                ),
                            ),
                        ),
                    ],
                },
            ),
        )
        "###);
    }

    #[test]
    fn test_parse_atomic_type() {
        assert_debug_snapshot!(parse_type("var"), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 3,
            },
            Var(
                Symbol(
                    "var",
                ),
            ),
        )
        "###);
        assert_debug_snapshot!(parse_type("\"string\""), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 8,
            },
            TypeLevelString(
                "string",
            ),
        )
        "###);
        assert_debug_snapshot!(parse_type("42"), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 2,
            },
            TypeLevelInt(
                42,
            ),
        )
        "###);
    }

    #[test]
    fn test_parse_complex_type() {
        assert_debug_snapshot!(parse_type("Maybe Int"), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 9,
            },
            TypeApp(
                Located(
                    SourceSpan {
                        start: 0,
                        end: 5,
                    },
                    TypeConstructor(
                        QualifiedName(
                            Symbol(
                                "Maybe",
                            ),
                        ),
                    ),
                ),
                Located(
                    SourceSpan {
                        start: 6,
                        end: 9,
                    },
                    TypeConstructor(
                        QualifiedName(
                            Symbol(
                                "Int",
                            ),
                        ),
                    ),
                ),
            ),
        )
        "###);
        assert_debug_snapshot!(parse_type("Either String Int"), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 17,
            },
            TypeApp(
                Located(
                    SourceSpan {
                        start: 0,
                        end: 13,
                    },
                    TypeApp(
                        Located(
                            SourceSpan {
                                start: 0,
                                end: 6,
                            },
                            TypeConstructor(
                                QualifiedName(
                                    Symbol(
                                        "Either",
                                    ),
                                ),
                            ),
                        ),
                        Located(
                            SourceSpan {
                                start: 7,
                                end: 13,
                            },
                            TypeConstructor(
                                QualifiedName(
                                    Symbol(
                                        "String",
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
                Located(
                    SourceSpan {
                        start: 14,
                        end: 17,
                    },
                    TypeConstructor(
                        QualifiedName(
                            Symbol(
                                "Int",
                            ),
                        ),
                    ),
                ),
            ),
        )
        "###);
        assert_debug_snapshot!(parse_type("Array (Maybe Int)"), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 17,
            },
            TypeApp(
                Located(
                    SourceSpan {
                        start: 0,
                        end: 5,
                    },
                    TypeConstructor(
                        QualifiedName(
                            Symbol(
                                "Array",
                            ),
                        ),
                    ),
                ),
                Located(
                    SourceSpan {
                        start: 6,
                        end: 17,
                    },
                    Parens(
                        Located(
                            SourceSpan {
                                start: 7,
                                end: 16,
                            },
                            TypeApp(
                                Located(
                                    SourceSpan {
                                        start: 7,
                                        end: 12,
                                    },
                                    TypeConstructor(
                                        QualifiedName(
                                            Symbol(
                                                "Maybe",
                                            ),
                                        ),
                                    ),
                                ),
                                Located(
                                    SourceSpan {
                                        start: 13,
                                        end: 16,
                                    },
                                    TypeConstructor(
                                        QualifiedName(
                                            Symbol(
                                                "Int",
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        )
        "###);
    }

    #[test]
    fn test_parse_forall() {
        assert_debug_snapshot!(parse_type("forall x (y :: Symbol). Maybe x"), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 31,
            },
            ForAll {
                vars: [
                    (
                        Symbol(
                            "x",
                        ),
                        None,
                    ),
                    (
                        Symbol(
                            "y",
                        ),
                        Some(
                            Located(
                                SourceSpan {
                                    start: 15,
                                    end: 21,
                                },
                                TypeConstructor(
                                    QualifiedName(
                                        Symbol(
                                            "Symbol",
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ],
                body: Located(
                    SourceSpan {
                        start: 24,
                        end: 31,
                    },
                    TypeApp(
                        Located(
                            SourceSpan {
                                start: 24,
                                end: 29,
                            },
                            TypeConstructor(
                                QualifiedName(
                                    Symbol(
                                        "Maybe",
                                    ),
                                ),
                            ),
                        ),
                        Located(
                            SourceSpan {
                                start: 30,
                                end: 31,
                            },
                            Var(
                                Symbol(
                                    "x",
                                ),
                            ),
                        ),
                    ),
                ),
                skolem_scope: None,
            },
        )
        "###);
    }

    #[test]
    fn test_parse_constraint() {
        assert_debug_snapshot!(parse_type("Eq a => a"), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 9,
            },
            Constrained {
                constraint: Located(
                    SourceSpan {
                        start: 0,
                        end: 4,
                    },
                    TypeApp(
                        Located(
                            SourceSpan {
                                start: 0,
                                end: 2,
                            },
                            TypeConstructor(
                                QualifiedName(
                                    Symbol(
                                        "Eq",
                                    ),
                                ),
                            ),
                        ),
                        Located(
                            SourceSpan {
                                start: 3,
                                end: 4,
                            },
                            Var(
                                Symbol(
                                    "a",
                                ),
                            ),
                        ),
                    ),
                ),
                body: Located(
                    SourceSpan {
                        start: 8,
                        end: 9,
                    },
                    Var(
                        Symbol(
                            "a",
                        ),
                    ),
                ),
            },
        )
        "###);
    }

    #[test]
    fn test_parse_constraints() {
        assert_debug_snapshot!(parse_type("Eq a => Show a => a"), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 19,
            },
            Constrained {
                constraint: Located(
                    SourceSpan {
                        start: 0,
                        end: 4,
                    },
                    TypeApp(
                        Located(
                            SourceSpan {
                                start: 0,
                                end: 2,
                            },
                            TypeConstructor(
                                QualifiedName(
                                    Symbol(
                                        "Eq",
                                    ),
                                ),
                            ),
                        ),
                        Located(
                            SourceSpan {
                                start: 3,
                                end: 4,
                            },
                            Var(
                                Symbol(
                                    "a",
                                ),
                            ),
                        ),
                    ),
                ),
                body: Located(
                    SourceSpan {
                        start: 8,
                        end: 19,
                    },
                    Constrained {
                        constraint: Located(
                            SourceSpan {
                                start: 8,
                                end: 14,
                            },
                            TypeApp(
                                Located(
                                    SourceSpan {
                                        start: 8,
                                        end: 12,
                                    },
                                    TypeConstructor(
                                        QualifiedName(
                                            Symbol(
                                                "Show",
                                            ),
                                        ),
                                    ),
                                ),
                                Located(
                                    SourceSpan {
                                        start: 13,
                                        end: 14,
                                    },
                                    Var(
                                        Symbol(
                                            "a",
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        body: Located(
                            SourceSpan {
                                start: 18,
                                end: 19,
                            },
                            Var(
                                Symbol(
                                    "a",
                                ),
                            ),
                        ),
                    },
                ),
            },
        )
        "###);
    }

    //
}
