# purs-oxide

This little thing someday hopes to become a PureScript compiler. For now it's just an incomplete parser, but it is growing steadily.

### Advantages

- Rewritten in `R U S T`

### Disadvantages

- Mostly vaporware for now

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

#### Development procedure

1. Find some random (valid) PureScript code
2. `cargo run < random.purs`
3. Figure out why it doesn't parse
4. Implement the missing functionality
5. Repeat
6. ...
7. Profit!

### Later compilation stages

There's no trace yet of desugaring, renaming, typechecking or code generation.

## Testing

We use [insta](https://docs.rs/insta/1.29.0/insta/) for snapshot testing.

The snapshots are currently inline, but the parser outputs are getting large, and maintaining this is getting untenable.
