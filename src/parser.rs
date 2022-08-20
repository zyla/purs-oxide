use crate::ast::{Expr, Module, Type};
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

pub fn parse_expr(input: &str) -> ParseResult<Expr> {
    let mut errors = vec![];
    let lexer = lexer::lex(input);
    let result = parser::ExprParser::new().parse(&mut errors, lexer);
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
    fn parse_expr(input: &str) -> crate::ast::Expr {
        expect_success(super::parse_expr(input))
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
                    name: QualifiedName(
                        Symbol(
                            "Foo",
                        ),
                    ),
                    exports: None,
                    declarations: [],
                },
            ),
        )
        "###);
    }

    #[test]
    fn test_module_header_qualified() {
        assert_debug_snapshot!(parse_module(indoc!("
        module Some.Module where
        ")), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 24,
            },
            Commented(
                [],
                ModuleInner {
                    name: QualifiedName(
                        Symbol(
                            "Some.Module",
                        ),
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
                    name: QualifiedName(
                        Symbol(
                            "Foo",
                        ),
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
        assert_debug_snapshot!(parse_type("Int"), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 3,
            },
            TypeConstructor(
                QualifiedName(
                    Symbol(
                        "Int",
                    ),
                ),
            ),
        )
        "###);
        assert_debug_snapshot!(parse_type("Prelude.Int"), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 11,
            },
            TypeConstructor(
                QualifiedName(
                    Symbol(
                        "Prelude.Int",
                    ),
                ),
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

    #[test]
    fn test_parse_row_1() {
        assert_debug_snapshot!(parse_type("( foo :: Int, \"Bar\" :: String )"), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 31,
            },
            Row {
                fields: [
                    (
                        Symbol(
                            "foo",
                        ),
                        Located(
                            SourceSpan {
                                start: 9,
                                end: 12,
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
                    (
                        Symbol(
                            "Bar",
                        ),
                        Located(
                            SourceSpan {
                                start: 23,
                                end: 29,
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
                ],
                rest: None,
            },
        )
        "###);
    }

    #[test]
    fn test_parse_row_2() {
        assert_debug_snapshot!(parse_type("( foo :: Int | e )"), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 18,
            },
            Row {
                fields: [
                    (
                        Symbol(
                            "foo",
                        ),
                        Located(
                            SourceSpan {
                                start: 9,
                                end: 12,
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
                ],
                rest: Some(
                    Located(
                        SourceSpan {
                            start: 15,
                            end: 16,
                        },
                        Var(
                            Symbol(
                                "e",
                            ),
                        ),
                    ),
                ),
            },
        )
        "###);
    }

    #[test]
    fn test_parse_row_3() {
        assert_debug_snapshot!(parse_type("( | e )"), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 7,
            },
            Row {
                fields: [],
                rest: Some(
                    Located(
                        SourceSpan {
                            start: 4,
                            end: 5,
                        },
                        Var(
                            Symbol(
                                "e",
                            ),
                        ),
                    ),
                ),
            },
        )
        "###);
    }

    #[test]
    fn test_parse_row_4() {
        assert_debug_snapshot!(parse_type("()"), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 2,
            },
            Row {
                fields: [],
                rest: None,
            },
        )
        "###);
    }

    #[test]
    fn test_parse_record() {
        assert_debug_snapshot!(parse_type("{ foo :: Int | e }"), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 18,
            },
            TypeApp(
                Located(
                    SourceSpan {
                        start: 0,
                        end: 1,
                    },
                    TypeConstructor(
                        QualifiedName(
                            Symbol(
                                "Prim.Record",
                            ),
                        ),
                    ),
                ),
                Located(
                    SourceSpan {
                        start: 2,
                        end: 16,
                    },
                    Row {
                        fields: [
                            (
                                Symbol(
                                    "foo",
                                ),
                                Located(
                                    SourceSpan {
                                        start: 9,
                                        end: 12,
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
                        ],
                        rest: Some(
                            Located(
                                SourceSpan {
                                    start: 15,
                                    end: 16,
                                },
                                Var(
                                    Symbol(
                                        "e",
                                    ),
                                ),
                            ),
                        ),
                    },
                ),
            ),
        )
        "###);
    }

    #[test]
    fn test_parse_literals() {
        assert_debug_snapshot!(parse_expr("123"), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 3,
            },
            Literal(
                Integer(
                    123,
                ),
            ),
        )
        "###);
        assert_debug_snapshot!(parse_expr(r#" "hello" "#), @r###"
        Located(
            SourceSpan {
                start: 1,
                end: 8,
            },
            Literal(
                String(
                    "hello",
                ),
            ),
        )
        "###);
        assert_debug_snapshot!(parse_expr(r#" true "#), @r###"
        Located(
            SourceSpan {
                start: 1,
                end: 5,
            },
            Literal(
                Boolean(
                    true,
                ),
            ),
        )
        "###);
        assert_debug_snapshot!(parse_expr(r#" 'a' "#), @r###"
        Located(
            SourceSpan {
                start: 1,
                end: 4,
            },
            Literal(
                Char(
                    'a',
                ),
            ),
        )
        "###);
    }

    #[test]
    fn test_parse_array() {
        assert_debug_snapshot!(parse_expr(r#" [] "#), @r###"
        Located(
            SourceSpan {
                start: 1,
                end: 3,
            },
            Literal(
                Array(
                    [],
                ),
            ),
        )
        "###);
        assert_debug_snapshot!(parse_expr(r#" [1] "#), @r###"
        Located(
            SourceSpan {
                start: 1,
                end: 4,
            },
            Literal(
                Array(
                    [
                        Located(
                            SourceSpan {
                                start: 2,
                                end: 3,
                            },
                            Literal(
                                Integer(
                                    1,
                                ),
                            ),
                        ),
                    ],
                ),
            ),
        )
        "###);
        assert_debug_snapshot!(parse_expr(r#" [true, false] "#), @r###"
        Located(
            SourceSpan {
                start: 1,
                end: 14,
            },
            Literal(
                Array(
                    [
                        Located(
                            SourceSpan {
                                start: 2,
                                end: 6,
                            },
                            Literal(
                                Boolean(
                                    true,
                                ),
                            ),
                        ),
                        Located(
                            SourceSpan {
                                start: 8,
                                end: 13,
                            },
                            Literal(
                                Boolean(
                                    false,
                                ),
                            ),
                        ),
                    ],
                ),
            ),
        )
        "###);
    }

    #[test]
    fn test_parse_record_expr() {
        assert_debug_snapshot!(parse_expr(r#" {} "#), @r###"
        Located(
            SourceSpan {
                start: 1,
                end: 3,
            },
            Literal(
                Object(
                    [],
                ),
            ),
        )
        "###);
        assert_debug_snapshot!(parse_expr(r#" { foo: 1 } "#), @r###"
        Located(
            SourceSpan {
                start: 1,
                end: 11,
            },
            Literal(
                Object(
                    [
                        (
                            Symbol(
                                "foo",
                            ),
                            Located(
                                SourceSpan {
                                    start: 8,
                                    end: 9,
                                },
                                Literal(
                                    Integer(
                                        1,
                                    ),
                                ),
                            ),
                        ),
                    ],
                ),
            ),
        )
        "###);
        assert_debug_snapshot!(parse_expr(r#" { foo } "#), @r###"
        Located(
            SourceSpan {
                start: 1,
                end: 8,
            },
            Literal(
                Object(
                    [
                        (
                            Symbol(
                                "foo",
                            ),
                            Located(
                                SourceSpan {
                                    start: 3,
                                    end: 6,
                                },
                                Var(
                                    QualifiedName(
                                        Symbol(
                                            "foo",
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ],
                ),
            ),
        )
        "###);
        assert_debug_snapshot!(parse_expr(r#" { foo, bar: 2 } "#), @r###"
        Located(
            SourceSpan {
                start: 1,
                end: 16,
            },
            Literal(
                Object(
                    [
                        (
                            Symbol(
                                "foo",
                            ),
                            Located(
                                SourceSpan {
                                    start: 3,
                                    end: 6,
                                },
                                Var(
                                    QualifiedName(
                                        Symbol(
                                            "foo",
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        (
                            Symbol(
                                "bar",
                            ),
                            Located(
                                SourceSpan {
                                    start: 13,
                                    end: 14,
                                },
                                Literal(
                                    Integer(
                                        2,
                                    ),
                                ),
                            ),
                        ),
                    ],
                ),
            ),
        )
        "###);
    }

    #[test]
    fn test_parse_infix_expr() {
        assert_debug_snapshot!(parse_expr(r#" 1 %+ 2 <$> 3 "#), @r###"
        Located(
            SourceSpan {
                start: 1,
                end: 13,
            },
            Infix(
                Located(
                    SourceSpan {
                        start: 1,
                        end: 2,
                    },
                    Literal(
                        Integer(
                            1,
                        ),
                    ),
                ),
                [
                    (
                        Symbol(
                            "%+",
                        ),
                        Located(
                            SourceSpan {
                                start: 6,
                                end: 7,
                            },
                            Literal(
                                Integer(
                                    2,
                                ),
                            ),
                        ),
                    ),
                    (
                        Symbol(
                            "<$>",
                        ),
                        Located(
                            SourceSpan {
                                start: 12,
                                end: 13,
                            },
                            Literal(
                                Integer(
                                    3,
                                ),
                            ),
                        ),
                    ),
                ],
            ),
        )
        "###);
    }

    #[test]
    fn test_parse_accessor_1() {
        assert_debug_snapshot!(parse_expr(r#"foo.bar"#), @r###"
        Located(
            SourceSpan {
                start: 0,
                end: 7,
            },
            Accessor(
                Located(
                    SourceSpan {
                        start: 0,
                        end: 3,
                    },
                    Var(
                        QualifiedName(
                            Symbol(
                                "foo",
                            ),
                        ),
                    ),
                ),
                Symbol(
                    "bar",
                ),
            ),
        )
        "###);
    }

    #[test]
    fn test_parse_accessor_2() {
        assert_debug_snapshot!(parse_expr(r#" foo."Bar" "#), @r###"
        Located(
            SourceSpan {
                start: 1,
                end: 10,
            },
            Accessor(
                Located(
                    SourceSpan {
                        start: 1,
                        end: 4,
                    },
                    Var(
                        QualifiedName(
                            Symbol(
                                "foo",
                            ),
                        ),
                    ),
                ),
                Symbol(
                    "Bar",
                ),
            ),
        )
        "###);
    }

    #[test]
    fn test_parse_accessor_chain() {
        assert_debug_snapshot!(parse_expr(r#" foo.bar.baz "#), @r###"
        Located(
            SourceSpan {
                start: 1,
                end: 12,
            },
            Accessor(
                Located(
                    SourceSpan {
                        start: 1,
                        end: 8,
                    },
                    Accessor(
                        Located(
                            SourceSpan {
                                start: 1,
                                end: 4,
                            },
                            Var(
                                QualifiedName(
                                    Symbol(
                                        "foo",
                                    ),
                                ),
                            ),
                        ),
                        Symbol(
                            "bar",
                        ),
                    ),
                ),
                Symbol(
                    "baz",
                ),
            ),
        )
        "###);
    }

    //
}
