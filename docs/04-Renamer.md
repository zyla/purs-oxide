# Renamer (Name Resolution)

## Overview
The renaming phase resolves all names in the AST to their absolute definitions. It handles imports, exports, scoping, and computes Strongly Connected Components (SCCs) for mutually recursive declarations.

## Current Status
🚧 **IN PROGRESS** - This is the current focus of development

## Core Responsibilities

### 1. Name Resolution
Convert all names from "as written" to absolute references:
```purescript
-- Before: names as strings
import Data.Maybe (Maybe)
foo = Just 42

-- After: names as absolute references
import Data.Maybe (Maybe)  -- Maybe → Data.Maybe.Maybe
foo = Just 42              -- Just → Data.Maybe.Just
```

### 2. Import Resolution
- Process import statements
- Build imported names table
- Handle qualified imports
- Check for import conflicts

### 3. Export Resolution
- Process export lists (or implicit exports)
- Handle re-exports
- Build module interface

### 4. SCC Computation
Find mutually recursive declaration groups:
```purescript
-- These form an SCC (must be type-checked together)
foo = bar 42
bar x = foo (x + 1)
```

## Incremental Design

### Query Structure
```rust
// Main renaming query - processes whole module
renamed_module(db, module_id) -> RenamedModule

// Helper queries for incremental import/export resolution
imported_decls(db, module_id) -> Vec<(Option<ModuleId>, DeclId)>
exported_decls(db, module_id) -> Vec<DeclId>

// Extract single declaration after renaming
renamed_value_decl(db, decl_id) -> ValueDecl
```

### Why Rename Whole Module?
1. **SCC Computation**: Needs to see all declarations
2. **Local Scoping**: Names in one declaration affect others
3. **Import Context**: All declarations share same imports

### Incremental Benefits
- Import resolution cached separately
- Export resolution cached separately
- When imports change but code doesn't, only re-resolve names
- When declaration changes, only affected SCCs invalidated

## Name Resolution Algorithm

### Scoping Rules
1. **Local bindings** (innermost first)
2. **Declaration parameters**
3. **Module-level declarations**
4. **Imported names**
5. **Implicitly imported Prelude/Prim**

### Resolution Process
```rust
struct Renamer {
    module_scope: HashMap<QualifiedName, AbsoluteName>,
    local_scopes: Vec<HashSet<Symbol>>,  // Stack of scopes
}
```

1. Build module scope from imports + local declarations
2. Walk AST, maintaining scope stack
3. Resolve each name to absolute reference
4. Report unresolved names as errors

## SCC Computation

### Why SCCs Matter
Type inference needs to process mutually recursive declarations together:
```purescript
-- Must infer types together
even 0 = True
even n = odd (n - 1)
odd 0 = False
odd n = even (n - 1)
```

### SCC Algorithm
1. Build dependency graph between declarations
2. Run Tarjan's SCC algorithm
3. Assign stable SCC IDs (smallest DeclId in group)

### Incremental Considerations
- SCC membership can change when dependencies change
- Stable IDs minimize cache invalidation
- Track "dirty" declarations within SCCs?

## Current Implementation Gaps

### Missing Features
1. **Desugaring**: Should happen here or separate phase?
   - Pattern guards → case expressions
   - Do notation → bind calls
   - Where clauses → let expressions

2. **Shadow Detection**:
   ```purescript
   foo x = let x = 42 in x  -- Should warn about shadowing
   ```

3. **Operator Resolution**:
   - Infix operators to function calls
   - Precedence/associativity

4. **Type Constructor Resolution**:
   - Currently only handles value names
   - Need to resolve type names too

## Data Flow

```
IndexedModule
    ↓
[Import Resolution] - Cached separately
    ↓
[Build Module Scope]
    ↓
[Rename Declarations] - Walk AST resolving names
    ↓
[SCC Computation]
    ↓
RenamedModule (all names absolute, SCCs computed)
```

## Requirements for Completion

### High Priority
1. **Complete Name Resolution**:
   - Type names
   - Class names
   - Operators

2. **Proper Scoping**:
   - Pattern bindings
   - Where clauses
   - Let bindings
   - Lambda parameters

3. **Better Error Messages**:
   - Undefined name suggestions
   - Import conflict details
   - Shadowing warnings

### Medium Priority
1. **Desugaring**: Where should this happen?
2. **Optimization**: Faster name lookup structures
3. **Validation**: Check for orphan instances

## Design Questions

1. **Desugaring Placement**:
   - During renaming (current plan)?
   - Separate phase after renaming?
   - During type checking?

2. **Error Recovery**:
   - Use "unknown" marker for unresolved names?
   - Continue renaming after errors?
   - Partial results for IDE?

3. **Incremental Granularity**:
   - Can we rename individual SCCs?
   - Track fine-grained dependencies?
   - Worth the complexity?

4. **Instance Resolution**:
   - Build instance tables here?
   - Just collect instances?
   - Defer to type checking?

## Testing Strategy

- Test all scoping rules
- Test with complex mutual recursion
- Ensure SCC computation is correct
- Test import/export edge cases
- Measure performance on large modules