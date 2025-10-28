# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

purs-oxide is a PureScript compiler written in Rust, currently focusing on parser and renamer implementation. It uses [Salsa](https://github.com/salsa-rs/salsa) for incremental compilation and [LALRPOP](https://lalrpop.github.io/lalrpop/) for parser generation.

**Current Status:** Parser can parse real PureScript code (including the Restaumatic codebase). Currently working on the renamer. No typechecker or code generation yet (although basic codegen exists for bundling).

## Build and Development Commands

### Building
```bash
cargo build
cargo build --release
```

### Testing
```bash
# Run all tests
cargo test

# Run tests with logging
RUST_LOG=debug cargo test

# Run specific test
cargo test test_compile_pass

# Update snapshots (for insta snapshot tests)
cargo insta review
cargo insta accept
```

### Running the Compiler
```bash
# Parse files
cargo run -- parse <FILES>

# Bundle with entrypoint
cargo run -- bundle --entrypoint Module.function <FILES>
```

### Code Formatting
```bash
# Check formatting
cargo fmt --check

# Format code
cargo fmt
```

### Linting
```bash
cargo clippy
cargo clippy --all-targets --all-features
```

### Pre-commit Hook
Install the pre-commit hook for automatic formatting:
```bash
bash install-hooks.sh
```

## Architecture

### Incremental Compilation with Salsa

The compiler is structured as a graph of Salsa queries rather than a traditional pipeline. This enables incremental compilation where only affected parts are recompiled.

**Key Design Decisions:**
- Source files are keyed by module name (via `ModuleId`), not filename
- Parsing happens at the module granularity (whole file at once)
- Later passes work at declaration granularity for better incrementality
- The `Database` struct (defined in `src/lib.rs`) is the main Salsa database

### Module Organization

```
src/
├── ast/              # AST types (declarations, expressions, types)
├── lexer.rs          # Handwritten lexer with layout handling
├── parser.lalrpop    # LALRPOP parser grammar
├── parser.rs         # Generated parser (from .lalrpop)
├── indexed_module.rs # Post-processed AST with declarations by name
├── rename.rs         # Name resolution logic
├── renamed_module.rs # Renamed/resolved AST
├── scc.rs            # Strongly connected components for mutual recursion
├── typecheck.rs      # Type checking (WIP)
├── codegen.rs        # Code generation for bundling
├── lib.rs            # Main database and Salsa setup
└── main.rs           # CLI entry point
```

### Compilation Pipeline (Salsa Queries)

1. **`parsed_module(ModuleId) -> ParsedModule`**
   - Raw AST from parsing source file
   - Entry point: source files keyed by module name

2. **`indexed_module(ModuleId) -> IndexedModule`**
   - Post-processes raw AST
   - Collects declarations by name
   - Spans are relative to declaration start (for better incrementality)

3. **`renamed_module(ModuleId) -> RenamedModule`**
   - Desugaring (lowering complex constructs)
   - Name resolution (using imports/exports)
   - SCC computation for mutual recursion
   - Uses helper queries: `imported_decls`, `exported_decls`

4. **`typecheck_module(ModuleId)`** (WIP)
   - Type checks SCCs (mutually recursive groups)
   - Uses `typechecked_scc(SccId)` for actual checking

### Key Types

**`ModuleId`** - Salsa-interned identifier for a module (by name string)

**`DeclId`** - Reference to a declaration:
```rust
struct DeclId {
    namespace: Namespace,  // Class, Type, or Value
    module: ModuleId,
    name: Symbol,
}
```

**`Symbol`** - String wrapper, intended for future string interning

**`SccId`** - Identifier for a strongly connected component (mutually recursive declaration group). The ID is the lexicographically smallest DeclId in the group for stability.

### Special Handling

**Layout Rules:** The lexer handles PureScript's layout-sensitive syntax. This is complex and hand-written (see `src/lexer.rs`).

**Tokens:** Currently store `String`s instead of slices, so they are not `Copy` and allocate frequently.

**Built-in Prelude:** The `Database::new_with_prelude()` automatically loads PureScript prelude from `prelude/src/`.

**Prim Module:** Automatically added to all databases via `src/prim.rs`.

## Testing Strategy

Uses [insta](https://docs.rs/insta/) for snapshot testing. Test files are in:
- `tests/purs/compile_pass/*.purs` - Should compile without errors
- `tests/purs/compile_fail/*.purs` - Should fail with errors

Snapshots stored in `src/snapshots/`.

When adding/changing tests:
1. Run `cargo test` to generate new snapshots
2. Review with `cargo insta review`
3. Accept with `cargo insta accept`

## Important Notes

**Parser Generation:** `src/parser.rs` is generated from `src/parser.lalrpop` by LALRPOP at build time (see `build.rs`). Never edit `parser.rs` directly - edit the `.lalrpop` file.

**Rust Version:** Pinned to 1.87.0 (see `rust-toolchain.toml`)

**Salsa Fork:** Uses a custom Salsa fork with derive-debug support (`branch = "derive-debug-withdb"`)

**Error Handling:** Diagnostics use the `Diagnostics` Salsa accumulator pattern. Errors are collected during queries and can be retrieved with `accumulated::<Diagnostics>()`.

**Parallel Compilation:** The database implements `ParallelDatabase` for parallel query execution (see `rayon` usage in `main.rs`).
