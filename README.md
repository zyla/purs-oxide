# purs-oxide

This little thing someday hopes to become a PureScript compiler. For now it's just a parser, but it is growing steadily.

## Pre-commit Hook

We have provided a pre-commit hook to ensure consistent code formatting using `cargo fmt`. Before committing changes, make sure your code is properly formatted. To set up the pre-commit hook, follow these steps:

1. Navigate to the root directory of the repository.

2. Install the pre-commit hook by running:

    ```bash
    bash hooks/install-hooks.sh
    ```

This will copy the pre-commit hook to your `.git/hooks` directory. Now, `cargo fmt` will automatically check your code before each commit.

If you encounter any issues or have questions, please don't hesitate to reach out.

### Advantages

- Rewritten in `R U S T`

### Disadvantages

- Mostly vaporware for now

## Development status

The parser understands some real code (can parse the Restaumatic codebase and its dependencies).

Currently we're working on the renamer.

If you're interested, there's a [development channel](https://app.gitter.im/#/room/!dvRAwXOtlcqHYqTaYW:gitter.im) on Matrix/Gitter.

## Architecture

### Incremental compilation

We're using [salsa](https://github.com/salsa-rs/salsa) for incremental compilation. 

Parsing is not incremental, whole file is parsed at a time. Later passes will be mostly done at definition granularity.

Our base inputs are source files, but keyed by module name, not file name. This is because file names essentially don't matter for PureScript[^1], so later passes refer to inputs by module name.

[^1]: Except for FFI, which we currently don't handle.

### Lexer

Handwritten, mainly due to layout handling.

Some issues there:

- The layout rules are quite complicated and I frankly don't understand how this works anymore, but it seems to pass the [layout test suite](https://github.com/purescript/purescript/tree/master/tests/purs/layout) from the original compiler.

- Tokens store `String`s instead of slices, so they are not Copy and we allocate a lot of stuff.

### AST

Nothing fancy, a tree linked together by `Box`es.

There is a dedicated `Symbol` type meant to introduce string interning later, but for now it's just a wrapper over `String`.

### Parser

Generated using the excellent [LALRPOP](https://lalrpop.github.io/lalrpop/) parser generator.

Super incomplete. This is the current most intense area of development.

### Later compilation stages

There's no trace yet of desugaring, renaming, typechecking or code generation.

## Testing

We use [insta](https://docs.rs/insta/1.29.0/insta/) for snapshot testing.
