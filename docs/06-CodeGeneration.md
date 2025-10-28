# Code Generation

## Overview
Code generation transforms the type-checked AST into executable code. Currently targeting JavaScript, but architecture should support multiple backends (WASM, native, etc.).

## Current Status
⚠️ **VERY BASIC** - Only simple bundling exists, no proper module system

## Compilation Modes

### 1. Module Mode
Traditional compilation - each PureScript module becomes a JS module:
```
Foo.purs → output/Foo/index.js
Bar.purs → output/Bar/index.js
```

**Status**: ❌ Not implemented

### 2. Bundle Mode
Dead code elimination from entry point:
```
All modules + entry point → single bundle.js
```

**Status**: ⚠️ Basic implementation exists

## Architecture Requirements

### Backend Abstraction
Need intermediate representation (IR) for multiple backends:
```
Type-Checked AST
    ↓
[Lower to IR] - Common for all backends
    ↓
Core IR (simplified, explicit)
    ↓
[Backend-specific codegen]
    ├─→ JavaScript
    ├─→ WASM
    └─→ Native
```

### Core IR Design
Should include:
1. **Explicit Dictionary Passing**: Type classes resolved
2. **Explicit Recursion**: Recursive bindings marked
3. **Simplified Patterns**: Pattern matching compiled to switches
4. **No Sugar**: All syntax sugar removed
5. **Explicit Effects**: Effect tracking if needed

## JavaScript Code Generation

### Current Approach
Direct AST to JS string (too simple):
```rust
fn expr_to_js(expr: &Expr) -> String {
    match expr {
        Literal(Int(n)) => n.to_string(),
        Lambda(param, body) =>
            format!("({}) => {}", param, expr_to_js(body)),
        // ...
    }
}
```

### Needed: Proper Module System

#### ES Modules
```javascript
// Foo/index.js
export const foo = 42;
export const bar = (x) => x + 1;

// Bar/index.js
import { foo } from '../Foo/index.js';
export const baz = foo + 1;
```

#### Bundle Format
```javascript
// All in one file, no module system
const Foo$foo = 42;
const Foo$bar = (x) => x + 1;
const Bar$baz = Foo$foo + 1;
```

## FFI Integration

### The Challenge
PureScript allows Foreign Function Interface to JavaScript:
```purescript
-- Foo.purs
foreign import addJS :: Int -> Int -> Int

-- Foo.js
export const addJS = (x) => (y) => x + y;
```

### Problems
1. **Name Conflicts**: FFI might conflict when bundling
2. **Module Boundaries**: FFI assumes module structure
3. **Side Effects**: FFI might have initialization code

### FFI Strategy (Decided)

#### Phase 1: Trust and Copy (MVP)
- Copy FFI files as-is
- No parsing or transformation
- Simple to implement, gets us running quickly
- Document naming conventions to avoid conflicts

#### Phase 2: Parse and Tree-Shake (Future)
- Parse JavaScript to understand exports
- Only include actually used FFI functions
- Eliminate dead code from FFI files
- Use JavaScript parser (swc or esbuild's parser)

#### Bundling with esbuild
After generating our bundle, use esbuild as a subprocess for:
- Dead code elimination
- Minification
- Tree shaking
- Modern optimization techniques

```bash
# Call esbuild as subprocess
esbuild bundle.js --bundle --minify --tree-shaking=true
```

This gives us professional bundling without reimplementing complex optimizations.

## Incremental Compilation

### Caching Strategy
```rust
// Cache generated code per SCC
scc_code(db, scc_id) -> String

// Bundle aggregates from SCCs
bundle(db, entry_point) -> String {
    let reachable_sccs = find_reachable(entry_point);
    reachable_sccs.map(|scc| scc_code(db, scc)).join("\n")
}
```

### Invalidation
- Type checking changes → regenerate code
- Only affected SCCs regenerated
- Bundle rebuilds by concatenating cached SCC code

## Optimizations

### Dead Code Elimination
Current: Tree-shaking from entry point
```rust
fn find_reachable(entry: DeclId) -> Set<DeclId> {
    // Traverse dependency graph from entry
    // Include only reachable declarations
}
```

### Potential Optimizations
1. **Inlining**: Small functions
2. **Constant Folding**: Compile-time evaluation
3. **TCO**: Tail call optimization
4. **Uncurrying**: Transform curried to multi-arg functions

## Requirements for Completion

### Critical (for FFI)
1. **FFI Loading**: Read and integrate `.js` files
2. **Module System**: Proper import/export
3. **Name Mangling**: Avoid conflicts in bundles

### Important (for Bundler)
1. **Source Maps**: Debugging support
2. **Tree Shaking**: Better dead code elimination
3. **Module Format**: CommonJS vs ES modules
4. **Minification**: Optional output minification

### Nice to Have
1. **Multiple Backends**: WASM, native
2. **Optimization Passes**: Inlining, etc.
3. **Lazy Evaluation**: For lazy data structures
4. **Worker Support**: Web worker code splitting

## Design Questions

### 1. Module Format
- **ES Modules**: Modern, tree-shakeable, but requires recent Node
- **CommonJS**: Compatible, but less optimizable
- **Both**: Configurable output format?

### 2. FFI Handling
- Parse and transform JS?
- Require specific format?
- Trust user to avoid conflicts?

### 3. Optimization Level
- How much optimization in compiler vs delegating to JS tools?
- Worth implementing own optimizer?
- Focus on correct code first?

### 4. Backend Architecture
- When to introduce IR?
- How to make backends pluggable?
- Shared vs backend-specific optimizations?

## Performance Targets

### Compilation Speed
- Should be near-instant for single module
- Bundle time proportional to reachable code
- Incremental bundling under 100ms for small changes

### Output Quality
- Readable for debugging (with source maps)
- Comparable size to original compiler
- Fast execution (no unnecessary overhead)

## Testing Strategy

1. **Correctness**: Output runs and produces correct results
2. **Module Tests**: Each compilation mode works
3. **FFI Tests**: Foreign imports work correctly
4. **Bundle Tests**: Tree-shaking removes unused code
5. **Performance**: Benchmark against original compiler