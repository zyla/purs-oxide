use crate::ast::InfixOp;
use crate::ast::Literal;
use crate::ast::Located;
use crate::ast::Pat;
use crate::ast::PatKind;
use crate::ast::TypeParameter;
use crate::ast::{Expr, ExprKind, Module, Type};
use crate::ast::{QualifiedName, TypeKind};
use crate::errors::Error;
use crate::lexer;
use crate::symbol::Symbol;
use crate::token::Token;
use crate::ModuleId;
use lalrpop_util::ErrorRecovery;
use lalrpop_util::ParseError;

lalrpop_mod!(pub parser);

pub(self) fn constraint_to_instance_head(c: Type) -> Option<(QualifiedName, Vec<Type>)> {
    let mut t = c;
    let mut args = vec![];
    loop {
        match t.into_inner() {
            TypeKind::TypeConstructor(con) => {
                args.reverse();
                return Some((con, args));
            }
            TypeKind::TypeApp(f, x) => {
                args.push(*x);
                t = *f;
            }
            _ => return None,
        }
    }
}

pub(self) fn constraint_to_class_head(
    db: &dyn crate::Db,
    c: Type,
) -> Option<(Symbol, Vec<TypeParameter>)> {
    let mut t = c;
    let mut params = vec![];
    loop {
        match t.into_inner() {
            TypeKind::TypeConstructor(con) => {
                if con.is_actually_qualified(db) {
                    return None;
                }
                params.reverse();
                return Some((con.name(db), params));
            }
            TypeKind::TypeApp(f, x) => match *x {
                Located(_, TypeKind::Var(v)) => {
                    params.push((v, None));
                    t = *f;
                }
                Located(
                    _,
                    TypeKind::Row {
                        mut fields,
                        rest: None,
                    },
                ) if fields.len() == 1 => {
                    let (v, k) = fields.pop().expect("should be non-empty");
                    params.push((v, Some(k)));
                    t = *f;
                }
                _ => return None,
            },
            _ => return None,
        }
    }
}

pub(self) fn apply_record_updates(f: Expr, args: Vec<Expr>) -> ExprKind {
    let mut result = vec![f];
    for expr in args {
        match expr {
            Located(suffix_span, ExprKind::RecordUpdateSuffix(update)) => {
                let last = result.pop().expect("should be non-empty");
                result.push(Located(
                    suffix_span,
                    ExprKind::RecordUpdate(Box::new(last), update),
                ));
            }
            _ => result.push(expr),
        }
    }
    let f = result.remove(0);
    ExprKind::App(Box::new(f), result)
}

pub(self) fn expr_to_pat(db: &dyn crate::Db, expr: Expr) -> Result<Pat, String> {
    let Located(span, kind) = expr;
    Ok(Located(
        span,
        match kind {
            ExprKind::Literal(lit) => PatKind::Literal(lit_expr_to_pat(db, lit)?),
            ExprKind::Infix(x, xs) => PatKind::Infix(
                Box::new(expr_to_pat(db, *x)?),
                xs.into_iter()
                    .map(|(op, x)| Ok::<_, String>((infix_op_to_pat(op)?, expr_to_pat(db, x)?)))
                    .collect::<Result<_, _>>()?,
            ),
            ExprKind::Accessor(_, _) => return Err("Illegal record accessor in pattern".into()),
            ExprKind::RecordUpdate(_, _) => return Err("Illegal record update in pattern".into()),
            ExprKind::Var(name) => {
                if name.is_actually_qualified(db) {
                    return Err("Illegal qualified name in pattern".into());
                } else {
                    PatKind::Var(name.name(db))
                }
            }
            ExprKind::DataConstructor(name) => PatKind::DataConstructorApp(name, vec![]),
            ExprKind::App(f, args) => match f.into_inner() {
                ExprKind::DataConstructor(name) => PatKind::DataConstructorApp(
                    name,
                    args.into_iter()
                        .map(|x| expr_to_pat(db, x))
                        .collect::<Result<_, _>>()?,
                ),
                f => {
                    return Err(format!(
                        "illegal pattern in data constructor position: {:?}",
                        f
                    ))
                }
            },
            ExprKind::Lam(_, _) => return Err("Illegal lambda in pattern".into()),
            ExprKind::Case { .. } => return Err("Illegal case in pattern".into()),
            ExprKind::If { .. } => return Err("Illegal if in pattern".into()),
            ExprKind::Typed(x, ty) => PatKind::Typed(Box::new(expr_to_pat(db, *x)?), ty),
            ExprKind::Let { .. } => return Err("Illegal let in pattern".into()),
            ExprKind::Wildcard => PatKind::Wildcard,
            ExprKind::RecordUpdateSuffix(_) => {
                return Err("Illegal record update in pattern".into())
            }
            ExprKind::Do(_) => return Err("Illegal do in pattern".into()),
            ExprKind::Ado(_, _) => return Err("Illegal ado in pattern".into()),
            ExprKind::NamedPat(name, x) => PatKind::Named(name, Box::new(expr_to_pat(db, *x)?)),
            ExprKind::Operator(_) => return Err("Illegal operator in pattern".into()),
            ExprKind::Negate(x) => match x.into_inner() {
                ExprKind::Literal(Literal::Integer(x)) => PatKind::Literal(Literal::Integer(-x)),
                _ => return Err("Illegal negation in pattern".into()),
            },
            ExprKind::Error => PatKind::Error,
        },
    ))
}

pub(self) fn infix_op_to_pat(op: InfixOp) -> Result<QualifiedName, String> {
    match op {
        InfixOp::Symbol(s) => Ok(s),
        InfixOp::Backtick(_) => Err("Illegal backtick operator in pattern".into()),
    }
}

pub(self) fn lit_expr_to_pat(
    db: &dyn crate::Db,
    lit: Literal<Expr>,
) -> Result<Literal<Pat>, String> {
    Ok(match lit {
        Literal::Integer(x) => Literal::Integer(x),
        Literal::Float(x) => Literal::Float(x),
        Literal::String(x) => Literal::String(x),
        Literal::Char(x) => Literal::Char(x),
        Literal::Boolean(x) => Literal::Boolean(x),
        Literal::Array(xs) => Literal::Array(
            xs.into_iter()
                .map(|x| expr_to_pat(db, x))
                .collect::<Result<_, _>>()?,
        ),
        Literal::Object(xs) => Literal::Object(
            xs.into_iter()
                .map(|(k, x)| Ok::<_, String>((k, expr_to_pat(db, x)?)))
                .collect::<Result<_, _>>()?,
        ),
    })
}

pub(self) fn normalize_app(f: Expr, x: Expr) -> ExprKind {
    match f {
        Located(_, ExprKind::App(f0, mut args)) => {
            args.push(x);
            ExprKind::App(f0, args)
        }
        _ => ExprKind::App(Box::new(f), vec![x]),
    }
}

type ParseResult<'a, T> = (
    Vec<ErrorRecovery<usize, Token, &'a str>>,
    Result<T, ParseError<usize, Token, Error>>,
);

pub fn parse_module<'a>(
    db: &'a dyn crate::Db,
    input: &'a str,
    module: crate::ModuleId,
) -> ParseResult<'a, Module> {
    let mut errors = vec![];
    let lexer = lexer::lex(input);
    let result = parser::ModuleParser::new().parse(db, &mut errors, module, lexer);
    (errors, result)
}

pub fn parse_lower_qualified_ident<'a>(
    db: &'a dyn crate::Db,
    input: &'a str,
) -> ParseResult<'a, QualifiedName> {
    let mut errors = vec![];
    let lexer = lexer::lex(input);
    let module = ModuleId::new(db, "Bundle".into());
    let result = parser::LowerQualifiedIdentParser::new().parse(db, &mut errors, module, lexer);
    (errors, result)
}

// Recognize `module Some.Module` header,
// without parsing the rest.
//
// Used for detecting which module a source file represents.
pub fn parse_module_name(input: &str) -> Option<String> {
    let mut lexer = lexer::lex(input);
    if !matches!(lexer.next(), Some(Ok((_, Token::Module, _)))) {
        return None;
    }
    match lexer.next() {
        Some(Ok((_, Token::UpperIdentifier(s), _))) => Some(s),
        Some(Ok((_, Token::QualifiedUpperIdentifier((prefix, suffix)), _))) => {
            Some(format!("{}.{}", prefix, suffix))
        }
        _ => None,
    }
}

pub fn parse_type<'a>(
    db: &'a dyn crate::Db,
    input: &'a str,
    module: crate::ModuleId,
) -> ParseResult<'a, Type> {
    let mut errors = vec![];
    let lexer = lexer::lex(input);
    let result = parser::TypeParser::new().parse(db, &mut errors, module, lexer);
    (errors, result)
}

pub fn parse_expr<'a>(
    db: &'a dyn crate::Db,
    input: &'a str,
    module: crate::ModuleId,
) -> ParseResult<'a, Expr> {
    let mut errors = vec![];
    let lexer = lexer::lex(input);
    let result = parser::ExprParser::new().parse(db, &mut errors, module, lexer);
    (errors, result)
}

#[cfg(test)]
mod tests {
    use crate::utils::tests::*;
    use indoc::indoc;
    use insta::{self, assert_debug_snapshot, assert_snapshot};

    fn expect_success<'db, T: salsa::DebugWithDb<<crate::Jar as ::salsa::jar::Jar<'db>>::DynDb>>(
        db: &<crate::Jar as ::salsa::jar::Jar<'db>>::DynDb,
        output: super::ParseResult<T>,
    ) -> String {
        let (errors, result) = output;
        assert_eq!(errors, &[]);
        let x = result.unwrap();
        format!("{:#?}", x.into_debug_all(db)).drop_salsa_id()
    }

    fn parse_module(input: &str) -> String {
        let db = crate::Database::new();
        let module = parse_module_id(input, &db);
        expect_success(&db, super::parse_module(&db, input, module))
    }
    fn parse_type(input: &str) -> String {
        let db = crate::Database::new();
        let module = dummy_module(&db);
        expect_success(&db, super::parse_type(&db, input, module))
    }
    fn parse_expr(input: &str) -> String {
        let db = crate::Database::new();
        let module = dummy_module(&db);
        expect_success(&db, super::parse_expr(&db, input, module))
    }

    #[test]
    fn test_module_header() {
        assert_snapshot!(parse_module(indoc!(
            "
        module Foo where
        "
        )));
    }

    #[test]
    fn test_module_name_parser() {
        assert_debug_snapshot!(super::parse_module_name(indoc!(
            "
        module Foo where
        import Bar
        "
        )));
    }

    #[test]
    fn test_module_name_parser_2() {
        assert_debug_snapshot!(super::parse_module_name(indoc!(
            "
        module Foo.Bar where
        import Bar
        "
        )));
    }

    #[test]
    fn test_module_header_qualified() {
        assert_snapshot!(parse_module(indoc!(
            "
        module Some.Module where
        "
        )));
    }

    #[test]
    fn test_simple_value_decl() {
        assert_snapshot!(parse_module(indoc!(
            "
        module Foo where
        x = 1
        "
        )));
    }

    #[test]
    fn test_typed_value_decl() {
        assert_snapshot!(parse_module(indoc!(
            "
        module Foo where
        x :: Int
        x = 1
        "
        )));
    }

    #[test]
    fn test_export_list() {
        assert_snapshot!(parse_module(indoc!(
            "
          module Control.Applicative
            ( class Applicative
            , pure
            , module Data.Functor
            , Either
            , Foo(..)
            , Maybe(Just, Nothing)
            , (+~)
            , type (<>)
            ) where

        "
        )));
    }

    #[test]
    fn test_imports() {
        assert_snapshot!(parse_module(indoc!(
            "
          module Test where

          import Foo.Asd
          import Bar.Asd as Baz
          import Qux.Asd (x)
          import Zzz.Asd (y, z) as Yyy
          import Aaa.Asd hiding (q)

          x = 1

        "
        )));
    }

    #[test]
    fn test_indented_where() {
        assert_snapshot!(parse_module(indoc!(
            "
            module Control.Applicative
              where
            import Control.Apply
        "
        )));
    }

    #[test]
    fn test_function_with_params() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            f x = 1
            g x y = 1
            h [x, y] = 1
            j {x, y: 1} = 1
            k "foo" = 1
            l 42 = 1
            m (x) = 1
        "#
        )));
    }

    #[test]
    fn test_type_synonym() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            type Foo = Int
            type Bar a = a
            type Baz a b = a
        "#
        )));
    }

    #[test]
    fn test_foreign_import() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            foreign import foo :: Int -> Int
        "#
        )));
    }

    #[test]
    fn test_typeclass_1() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            class Foo a where
              foo :: a -> Bool
              bar :: a
        "#
        )));
    }

    #[test]
    fn test_typeclass_2() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            class Bar a <= Foo a where
              bar :: a
        "#
        )));
    }

    #[test]
    fn test_typeclass_3() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            class (Bar a, Baz b) <= Foo a where
              bar :: a
        "#
        )));
    }

    #[test]
    fn test_typeclass_4() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            class Foo a where
        "#
        )));
    }

    #[test]
    fn test_typeclass_5() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            class Foo a
        "#
        )));
    }

    #[test]
    fn test_typeclass_var_kind() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            class Foo (a :: Symbol)
        "#
        )));
    }

    #[test]
    fn test_fundeps_1() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            class Foo a b | a -> b where
        "#
        )));
    }

    #[test]
    fn test_fundeps_2() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            class Foo a b c | a b -> c, a -> b c where
        "#
        )));
    }

    #[test]
    fn test_kind_signature_class() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            class Category :: forall k. (k -> k -> Type) -> Constraint
        "#
        )));
    }

    #[test]
    fn test_kind_signature_data() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            data Foo :: Type
            newtype Bar :: Type -> Type
        "#
        )));
    }

    #[test]
    fn test_kind_signature_type() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            type Qux :: Type -> Type
        "#
        )));
    }

    #[test]
    fn test_instance_1() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            instance Foo Int where
              foo x = 1
              bar = 2
        "#
        )));
    }

    #[test]
    fn test_instance_2() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            instance Bar a => Foo a where
              bar = 1
        "#
        )));
    }

    #[test]
    fn test_instance_3() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            instance (Bar a, Baz b) => Foo Int where
              bar = 1
        "#
        )));
    }

    #[test]
    fn test_instance_4() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            instance Foo Int where
        "#
        )));
    }

    #[test]
    fn test_instance_5() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            instance Foo Int
        "#
        )));
    }

    #[test]
    fn test_instance_6() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            instance namedInstance :: Foo Int where
        "#
        )));
    }

    #[test]
    fn test_instance_deriving() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            derive instance Foo Int
            derive newtype instance Foo Int
        "#
        )));
    }

    #[test]
    fn test_instance_chain() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            instance Foo Int where
              x = 1
            else instance Foo a where
              x = 2
        "#
        )));
    }

    #[test]
    fn test_instance_chain_2() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            instance Foo Int else
            instance Foo a
        "#
        )));
    }

    #[test]
    fn test_instance_method_sig() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            instance Foo Int where
                x :: Int
        "#
        )));
    }

    #[test]
    #[ignore = "Not implemented yet"]
    fn test_instance_method_infix() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            instance Semigroup Int where
                x <> y = z
        "#
        )));
    }

    #[test]
    fn test_data_decl() {
        assert_snapshot!(parse_module(indoc!(
            r#"
            module Test where
            data Maybe a = Nothing | Just a
            newtype Foo = Foo Int
            foreign import data X
            foreign import data X :: Type
        "#
        )));
    }

    #[test]
    fn test_parse_atomic_type() {
        assert_snapshot!(parse_type("var"));
        assert_snapshot!(parse_type("\"string\""));
        assert_snapshot!(parse_type("42"));
        assert_snapshot!(parse_type("Int"));
        assert_snapshot!(parse_type("Prelude.Int"));
        assert_snapshot!(parse_type("(-42)"));
    }

    #[test]
    fn test_parse_complex_type() {
        assert_snapshot!(parse_type("Maybe Int"));
        assert_snapshot!(parse_type("Either String Int"));
        assert_snapshot!(parse_type("Array (Maybe Int)"));
    }

    #[test]
    fn test_parse_forall() {
        assert_snapshot!(parse_type("forall x (y :: Symbol). Maybe x"));
    }

    #[test]
    fn test_parse_constraint() {
        assert_snapshot!(parse_type("Eq a => a"));
    }

    #[test]
    fn test_parse_constraints() {
        assert_snapshot!(parse_type("Eq a => Show a => a"));
    }

    #[test]
    fn test_parse_row_1() {
        assert_snapshot!(parse_type("( foo :: Int, \"Bar\" :: String, data :: Int )"));
    }

    #[test]
    fn test_parse_row_2() {
        assert_snapshot!(parse_type("( foo :: Int | e )"));
    }

    #[test]
    fn test_parse_row_3() {
        assert_snapshot!(parse_type("( | e )"));
    }

    #[test]
    fn test_parse_row_4() {
        assert_snapshot!(parse_type("()"));
    }

    #[test]
    fn test_parse_record() {
        assert_snapshot!(parse_type("{ foo :: Int | e }"));
    }

    #[test]
    fn test_parse_function_type() {
        assert_snapshot!(parse_type("A -> B -> C"));
    }

    #[test]
    fn test_function_as_type_operator() {
        assert_snapshot!(parse_type("(->)"));
    }

    #[test]
    fn test_type_operator() {
        assert_snapshot!(parse_type("a + b"));
    }

    #[test]
    fn test_type_operator_2() {
        assert_snapshot!(parse_type("a -> b ~> c -> d"));
    }

    #[test]
    fn test_type_operator_3() {
        assert_snapshot!(parse_type("a - b"));
    }

    #[test]
    fn test_type_operator_4() {
        assert_snapshot!(parse_type("a + b + c"));
    }

    #[test]
    fn test_parse_literals() {
        assert_snapshot!(parse_expr("123"));
        assert_snapshot!(parse_expr(r#" "hello" "#));
        assert_snapshot!(parse_expr(r#" true "#));
        assert_snapshot!(parse_expr(r#" 'a' "#));
    }

    #[test]
    fn test_float_literal() {
        assert_snapshot!(parse_expr("12.34"));
    }

    #[test]
    fn test_float_literal_with_underscores() {
        assert_snapshot!(parse_expr("15_000.0"));
    }

    #[test]
    fn test_parse_array() {
        assert_snapshot!(parse_expr(r#" [] "#));
        assert_snapshot!(parse_expr(r#" [1] "#));
        assert_snapshot!(parse_expr(r#" [true, false] "#));
    }

    #[test]
    fn test_parse_record_expr() {
        assert_snapshot!(parse_expr(r#" {} "#));
        assert_snapshot!(parse_expr(r#" { foo: 1 } "#));
        assert_snapshot!(parse_expr(r#" { foo } "#));
        assert_snapshot!(parse_expr(r#" { foo, bar: 2 } "#));
    }

    #[test]
    fn test_parse_infix_expr() {
        assert_snapshot!(parse_expr(r#" 1 %+ 2 <$> 3 "#));
    }

    #[test]
    fn test_infix_qualified() {
        assert_snapshot!(parse_expr(r#" x List.: xs "#));
    }

    #[test]
    fn test_parse_accessor_1() {
        assert_snapshot!(parse_expr(r#"foo.bar"#));
    }

    #[test]
    fn test_parse_accessor_2() {
        assert_snapshot!(parse_expr(r#" foo."Bar" "#));
    }

    #[test]
    fn test_parse_accessor_3() {
        assert_snapshot!(parse_expr(r#" foo.if "#));
    }

    #[test]
    fn test_parse_accessor_chain() {
        assert_snapshot!(parse_expr(r#" foo.bar.baz "#));
    }

    #[test]
    fn test_parse_qualified_var() {
        assert_snapshot!(parse_expr(r#"Data.Maybe.fromJust"#));
    }

    #[test]
    fn test_parse_parens() {
        assert_snapshot!(parse_expr(r#"(foo)"#));
    }

    #[test]
    fn test_parse_app_1() {
        assert_snapshot!(parse_expr(r#"f x y"#));
    }

    #[test]
    fn test_parse_app_2() {
        assert_snapshot!(parse_expr(r#"f a.b (g x)"#));
    }

    #[test]
    fn test_parse_lam_1() {
        assert_snapshot!(parse_expr(r#"\x -> y"#));
    }

    #[test]
    fn test_parse_lam_2() {
        assert_snapshot!(parse_expr(r#"\_ y -> y"#));
    }

    #[test]
    fn test_fat_arrows_as_operators() {
        assert_snapshot!(parse_expr(r#"1 <= 2 >= 3"#));
    }

    #[test]
    fn test_special_operators() {
        assert_snapshot!(parse_expr(r#"x : y"#));
    }

    #[test]
    fn test_case() {
        assert_snapshot!(parse_expr(indoc!(
            "
          case x of
            C a b ->
              1
            D (A c) _ -> 1
            E -> 1
            _ -> 1
        "
        )));
    }

    #[test]
    fn test_case_guards_1() {
        assert_snapshot!(parse_expr(indoc!(
            "
          case x of
            A | true -> 1
              | false -> 2
        "
        )));
    }

    #[test]
    fn test_case_guards_2() {
        assert_snapshot!(parse_expr(indoc!(
            "
          case x of
            A | true, Just x <- foo + bar -> 1
        "
        )));
    }

    #[test]
    fn test_case_multi() {
        assert_snapshot!(parse_expr(indoc!(
            "
          case x, y of
            C, D -> 1
        "
        )));
    }

    #[test]
    fn test_typed_expr() {
        assert_snapshot!(parse_expr("foo bar :: Int"));
    }

    #[test]
    fn test_if() {
        assert_snapshot!(parse_expr("if b then 1 else 2"));
    }

    #[test]
    fn test_let_1() {
        assert_snapshot!(parse_expr("let x = 1 in x"));
    }

    #[test]
    fn test_let_2() {
        assert_snapshot!(parse_expr(indoc!(
            "
            let
                x :: Int
                x = 1

                y = 2
                Tuple a b = y
            in \\z -> x + z
        "
        )));
    }

    #[test]
    fn test_let_guards() {
        assert_snapshot!(parse_expr(indoc!(
            "
            let x | true = 1
            in x
        "
        )));
    }

    #[test]
    fn test_let_guards_2() {
        assert_snapshot!(parse_expr(indoc!(
            "
            let Just x | true = 1
            in x
        "
        )));
    }

    #[test]
    fn test_wildcard() {
        assert_snapshot!(parse_expr("_.foo"));
    }

    #[test]
    fn test_data_con_expr() {
        assert_snapshot!(parse_expr("Just 1"));
    }

    #[test]
    fn test_block_argument() {
        assert_snapshot!(parse_expr("f \\x -> y"));
    }

    #[test]
    fn test_block_argument_2() {
        assert_snapshot!(parse_expr("f 1 \\x -> y"));
    }

    #[test]
    fn test_block_argument_3() {
        assert_snapshot!(parse_expr("f $ g \\x -> y"));
    }

    #[test]
    fn test_lambda_infix() {
        assert_snapshot!(parse_expr("1 + \\x -> y + 2"));
    }

    #[test]
    fn test_lambda_typed() {
        assert_snapshot!(parse_expr("\\x -> 1 :: Int"));
    }

    #[test]
    fn test_named_pattern() {
        assert_snapshot!(parse_expr("\\x@Nothing -> y"));
    }

    #[test]
    fn test_fn_arg_con_arity0() {
        assert_snapshot!(parse_module(indoc!(
            "
        module Some.Module where
        f Nothing = 1
        "
        )));
    }

    #[test]
    fn test_record_update_1() {
        assert_snapshot!(parse_expr("r { x = 1 }"));
    }

    #[test]
    fn test_record_update_2() {
        assert_snapshot!(parse_expr("f r { x = 1, y = 2, \"random label\" = 3 }"));
    }

    #[test]
    fn test_record_update_3() {
        assert_snapshot!(parse_expr("f r { x = 1 } { y: 2 } q"));
    }

    #[test]
    fn test_do_simple() {
        assert_snapshot!(parse_expr(indoc!(
            "
          do
            x <- f
            pure 1
        "
        )));
    }

    #[test]
    fn test_do_let() {
        assert_snapshot!(parse_expr(indoc!(
            "
          do
            let x = 1
            pure 2
        "
        )));
    }

    #[test]
    fn test_do_destructuring_pattern() {
        assert_snapshot!(parse_expr(indoc!(
            "
          do
            Tuple x y <- foo
            pure 2
        "
        )));
    }

    #[test]
    fn test_do_bind_type_sig() {
        assert_snapshot!(parse_expr(indoc!(
            "
          do
            x :: Int <- foo
            pure 2
        "
        )));
    }

    #[test]
    fn test_ado_simple() {
        assert_snapshot!(parse_expr(indoc!(
            "
          ado
            x <- f
            in 1
        "
        )));
    }

    #[test]
    fn test_ado_let() {
        assert_snapshot!(parse_expr(indoc!(
            "
          ado
            let x = 1
            in 2
        "
        )));
    }

    #[test]
    fn test_ado_full() {
        assert_snapshot!(parse_expr(indoc!(
            "
          ado
            let x = 1
            y <- f z
            g a
            in 2
        "
        )));
    }

    #[test]
    fn test_let_where() {
        assert_snapshot!(parse_expr(indoc!(
            "
        let x = y
              where y = 5
        in x
        "
        )));
    }

    #[test]
    fn test_case_where() {
        assert_snapshot!(parse_expr(indoc!(
            "
        case x of
            _ -> y
              where y = 5
        "
        )));
    }

    #[test]
    fn test_case_in_infix() {
        assert_snapshot!(parse_expr(indoc!(
            "
            case x of
                _ -> y
        <>
            case x of
                _ -> y
        "
        )));
    }

    #[test]
    fn test_standalone_operator() {
        assert_snapshot!(parse_expr("(+)"));
    }

    #[test]
    fn test_backtick_1() {
        assert_snapshot!(parse_expr("1 `mod` 2"));
    }

    #[test]
    fn test_backtick_2() {
        assert_snapshot!(parse_expr("1 `lift2 (+)` 2"));
    }

    // I think we can't reasonably support this with the current parser,
    // because then we could write:
    // 1 `\x y -> x `mod` y` 2
    //
    // and that is super ambiguous - at the second "`"` we can't decide whether to end the backtick
    // or start a new one. In fact these two parses would match:
    //
    // 1 `\x y -> (x `mod` y)` 2
    // (1 `\x y -> x` mod) `y` 2
    //
    // We could play some tricks in the layout parser, since it keeps track of backticks already
    // (weird!), but that would be even more complicated. I'm not sure there is code in the wild
    // doing this.
    //
    // And if you really want to, use parens, this works:
    // 1 `(\x y -> (x `mod` y))` 2
    #[test]
    #[ignore = "Super hard to implement, see comment"]
    fn test_backtick_3() {
        assert_snapshot!(parse_expr("1 `\\x y -> x` 2"));
    }

    #[test]
    fn test_backtick_4() {
        assert_snapshot!(parse_expr("1 `(\\x y -> x)` 2"));
    }

    // No idea why, but the original grammar explicitly permits infix operators
    // directly inside a backtick.
    #[test]
    fn test_backtick_5() {
        assert_snapshot!(parse_expr("1 `2 + 2` 2"));
    }

    #[test]
    fn test_negate_1() {
        assert_snapshot!(parse_expr("-5"));
    }

    #[test]
    fn test_negate_2() {
        assert_snapshot!(parse_expr("-f x"));
    }

    #[test]
    fn test_minus_op() {
        assert_snapshot!(parse_expr("x - y"));
    }

    #[test]
    fn test_operator_decl() {
        assert_snapshot!(parse_module(indoc!(
            "
        module Test where
        infix 1 f as !#
        infixl 2 g as $%#
        infixr 3 h as <@#%
        infix 1 Foo as ^%
        "
        )));
    }

    #[test]
    fn test_type_operator_decl() {
        assert_snapshot!(parse_module(indoc!(
            "
        module Test where
        infixr 4 type NaturalTransformation as ~>
        "
        )));
    }

    #[test]
    fn test_operator_decl_qualified() {
        assert_snapshot!(parse_module(indoc!(
            "
        module Test where
        infix 1 Foo.f as !#
        infix 2 Bar.X as ^%
        infix 3 type Bar.X as $^%
        "
        )));
    }

    #[test]
    fn test_role_decl() {
        assert_snapshot!(parse_module(indoc!(
            "
        module Test where
        type role Foo phantom representational nominal
        "
        )));
    }

    #[test]
    fn test_neg_pattern() {
        assert_snapshot!(parse_expr("case x of -1 -> 1"));
    }

    #[test]
    fn test_range_operator() {
        // Should not be confused with float literal and a dot
        assert_snapshot!(parse_expr("1..5"));
    }

    //
}
