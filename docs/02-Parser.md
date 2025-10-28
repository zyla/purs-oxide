# Parser

## Overview
The parser converts token streams into Abstract Syntax Trees (AST). Uses LALRPOP parser generator for maintainability and correctness.

## Current State
- ✅ Can parse real PureScript code (including Restaumatic codebase)
- ❌ Missing operator precedence/associativity handling
- ❌ Some syntax constructs incomplete
- ⚠️ Error recovery could be better

## Architecture Decisions

### Why LALRPOP?
- Declarative grammar specification
- Automatic parser generation
- Good error messages
- LR(1) parsing suitable for PureScript's grammar

### Grammar File
`src/parser.lalrpop` → generates → `src/parser.rs` (at build time)

## Incremental Compilation Considerations

### Parsing Granularity
- **Current**: Parse entire module at once
- **Result**: `Module` AST stored in Salsa as `ParsedModule`
- **Cache Key**: `ModuleId` (module name, not filename)

### Why Not More Incremental?
1. **Dependencies**: Declarations can reference each other
2. **Imports**: Need to see all imports at module top
3. **Exports**: Export list affects what's visible
4. **Complexity**: Sub-module parsing would require complex dependency tracking

### Optimization Opportunities
- Parse module once, then work at declaration level in later phases
- AST is immutable and shared via `Arc` in Salsa
- Changes to one declaration don't invalidate others (handled in next phase)

## AST Design

### Current Structure
```rust
struct Module {
    name: ModuleName,
    imports: Vec<Import>,
    exports: Option<Vec<Export>>,
    declarations: Vec<Declaration>,
}

enum Declaration {
    Value(ValueDecl),
    Type(TypeDecl),
    Class(ClassDecl),
    Instance(InstanceDecl),
    // ...
}
```

### Issues with Current AST
1. **Not Span-Preserving**: Should store spans for all nodes
2. **String Allocations**: Uses `String` everywhere instead of interned symbols
3. **No Comments**: Discards comments (problem for code formatting)

## Data Flow

```
Vec<Token>
    ↓
[LALRPOP Parser] - Module-level parsing
    ↓
ParsedModule { filename, ast: Module }
    ↓
[Indexing Phase]
```

## Missing Features

### High Priority
1. **Operator Precedence**: Need precedence/associativity table
   - Currently can't parse: `1 + 2 * 3` correctly
   - Requires separate precedence climbing or grammar refactoring

2. **Pattern Guards**: Complex pattern matching
   ```purescript
   case x of
     Just y | y > 0 -> ...
   ```

3. **Record Updates**:
   ```purescript
   record { field = newValue }
   ```

### Medium Priority
- Row polymorphism syntax
- Kind annotations
- Functional dependencies in classes
- Type operators

## Error Recovery Strategy

### Current
- LALRPOP's built-in error recovery
- Stops at first error

### Needed for IDE
- Continue parsing after errors
- Produce partial AST
- Multiple error reporting
- Synchronization points for recovery

## Requirements for Completion

### Must Have
1. **Operator Precedence**: Implement precedence climbing
2. **Complete Syntax**: All PureScript constructs
3. **Error Recovery**: Better recovery for IDE usage
4. **Benchmarking**: Parse time for large modules

### Nice to Have
1. **Incremental Parsing**: Tree-sitter style incremental parsing
2. **Parallel Parsing**: Parse multiple modules in parallel (already supported via Salsa)
3. **Comment Preservation**: For formatting tools

## Design Questions

1. **Operator Handling**:
   - In parser via precedence climbing?
   - Post-parse AST transformation?
   - Modified grammar with precedence levels?

2. **Error Recovery**:
   - Switch to hand-written recursive descent?
   - Extend LALRPOP with custom recovery?
   - Good enough as-is for compiler (not IDE)?

3. **AST Representation**:
   - When to intern strings?
   - How to preserve spans efficiently?
   - Separate AST types for each phase?

## Testing Strategy

- Parse all tests from original PureScript compiler
- Round-trip testing (parse → pretty-print → parse)
- Fuzzing with randomly generated code
- Real-world codebases (Restaumatic, Halogen, etc.)