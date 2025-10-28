# purs-oxide Compiler Documentation

## Overview
This directory contains detailed documentation for each phase of the purs-oxide compiler, a PureScript compiler written in Rust with focus on incremental compilation.

## Documentation Structure

### Core Concepts
- **[00-IncrementalCompilation.md](00-IncrementalCompilation.md)** - How Salsa enables true incremental compilation

### Compilation Phases
1. **[01-Lexer.md](01-Lexer.md)** - Tokenization and layout handling
2. **[02-Parser.md](02-Parser.md)** - LALRPOP-based parsing to AST
3. **[03-Indexing.md](03-Indexing.md)** - Declaration extraction and span adjustment
4. **[04-Renamer.md](04-Renamer.md)** - Name resolution and SCC computation
5. **[05-TypeChecking.md](05-TypeChecking.md)** - Type inference and checking
6. **[06-CodeGeneration.md](06-CodeGeneration.md)** - JavaScript output and bundling

### Infrastructure
7. **[07-Observability.md](07-Observability.md)** - Metrics, debugging, and performance monitoring

### Existing Architecture Doc
- **[Architecture.md](Architecture.md)** - Original design proposal (partially outdated)

## Current Status Summary

### ✅ Working
- **Lexer**: Complete with layout handling
- **Parser**: Can parse real code (Restaumatic codebase)
- **Basic Infrastructure**: Salsa integration, test framework

### 🚧 In Progress
- **Renamer**: Current development focus
- **Indexing**: Basic implementation exists

### ❌ TODO
- **Type Checking**: Structure exists but mostly unimplemented
- **Code Generation**: Only basic bundling
- **FFI Support**: Critical blocker
- **Operator Precedence**: Parser limitation

## Priority Roadmap

### Phase 1: MVP (Make it Work)
**Goal**: Compile simple PureScript programs

1. **FFI Support** [BLOCKER]
   - Read .js companion files
   - Integrate with module system
   - Handle in bundler

2. **Complete Renamer**
   - Finish name resolution
   - SCC computation
   - Basic desugaring

3. **Basic Type Checking**
   - Simple inference (no type classes yet)
   - Function types
   - Let polymorphism

4. **Bundler Improvements**
   - Proper module system
   - Tree shaking
   - Source maps

5. **Core Observability** [CRITICAL]
   - Basic metrics collection
   - `--explain-recompilation` flag
   - Prove incremental compilation works

### Phase 2: Feature Complete
**Goal**: Support real-world PureScript code

1. **Type Classes**
   - Instance resolution
   - Coherence checking
   - Dictionary transformation

2. **Advanced Types**
   - Row polymorphism
   - Higher-kinded types
   - Rank-N types

3. **Full Language Features**
   - All pattern matching
   - Guards
   - Do notation
   - Operator precedence

### Phase 3: Performance
**Goal**: Blazing fast incremental compilation

1. **Optimization**
   - Cache persistence
   - Parallel compilation
   - Query optimization

2. **IDE Support**
   - Sub-10ms query response
   - Error recovery
   - Partial compilation

3. **Advanced Codegen**
   - Multiple backends (WASM)
   - Optimizations (inlining, TCO)
   - Minification

## Key Design Principles

### 1. Incremental First
Every design decision prioritizes incremental compilation:
- Fine-grained dependency tracking
- Optimal cache invalidation
- Query-based architecture

### 2. Correctness Over Performance
Get it working correctly first:
- Match original compiler semantics
- Comprehensive testing
- Then optimize

### 3. Backend Agnostic
Design for multiple compilation targets:
- Abstract IR layer
- Pluggable backends
- Not JavaScript-specific

## Development Guidelines

### Adding New Features
1. Document the design first
2. Consider incremental impact
3. Write tests (use insta snapshots)
4. Benchmark against original compiler

### Testing
- Unit tests for each phase
- Integration tests using original compiler's test suite
- Snapshot testing for AST transformations
- Incremental behavior testing

## Benchmarking Goals

### Incremental Compilation
- **Target**: <100ms for typical changes
- **Current**: Not measurable (incomplete)
- **Original**: 2-5 seconds

### Full Compilation
- **Target**: Match or beat original
- **Current**: Not measurable (incomplete)
- **Original**: Baseline to establish

### Memory Usage
- **Target**: Linear with codebase size
- **Current**: Unknown
- **Concern**: String allocations in tokens

## Design Decisions (Resolved)

1. **FFI Strategy**: Phase 1 - Trust and copy; Phase 2 - Parse and tree-shake with esbuild
2. **Instance Tracking**: Method-level granularity for optimal bundle size
3. **Cache Persistence**: Store in `.purs-oxide-cache/` directory
4. **esbuild Integration**: Use as subprocess for bundling optimizations
5. **Backend Focus**: JavaScript first, keep IR design minimal but extensible

## Investigation Tasks

1. **Salsa Upgrade**: Investigate effort and benefits of upgrading from fork
   - When: During implementation if we hit limitations
   - Current: `branch = "derive-debug-withdb"`
   - Evaluate: Breaking changes, new features, performance improvements

## Open Questions for Discussion

1. **Error Recovery**: How important for MVP vs IDE support?
2. **Parser Architecture**: Stay with LALRPOP or switch for better incrementality?
3. **Type Class Coherence**: Global checking strategy?
4. **Operator Precedence**: Parser solution vs post-processing?

## Contributing

When working on purs-oxide:
1. Read relevant phase documentation
2. Understand incremental impact
3. Update documentation with changes
4. Discuss design decisions in issues

## Resources

- [Salsa Book](https://salsa-rs.github.io/salsa/) - Incremental computation framework
- [LALRPOP Book](https://lalrpop.github.io/lalrpop/) - Parser generator
- [PureScript Language Reference](https://github.com/purescript/documentation/tree/master/language) - Language specification