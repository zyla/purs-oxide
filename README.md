# purs-oxide

This little thing someday hopes to become a PureScript compiler. For now it's just a parser, but it is growing steadily.

### Advantages

- Rewritten in `R U S T`

### Disadvantages

- Mostly vaporware for now

## Development status

The parser understands some real code (can parse the Restaumatic codebase and its dependencies).

Currently we're working on integrating [salsa](https://github.com/salsa-rs/salsa) for incremental compilation. This is happening on the `salsa` branch in this repository.

If you're interested, there's a [development channel](https://app.gitter.im/#/room/!dvRAwXOtlcqHYqTaYW:gitter.im) on Matrix/Gitter.

## Architecture

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

The snapshots are currently inline, but the parser outputs are getting large, and maintaining this is getting untenable.
