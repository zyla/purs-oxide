# Indexing

## Overview
The indexing phase transforms the raw AST into a structure optimized for incremental compilation. It collects declarations by name and adjusts source spans to be relative to declaration starts.

## Purpose in Incremental Compilation

### The Problem
When you change line 1 of a file, all absolute spans in the AST change for every declaration below. This would invalidate all cached data for unchanged declarations.

### The Solution
Make spans relative to declaration starts:
- Each declaration tracks its absolute position in file
- Spans within declaration are relative to declaration start
- When declaration moves but content unchanged, relative spans stay same

## Current Implementation

### Input/Output
```rust
// Input
parsed_module(db, module_id) -> ParsedModule

// Output
indexed_module(db, module_id) -> IndexedModule
```

### IndexedModule Structure
```rust
struct IndexedModule {
    // Declarations indexed by name for fast lookup
    value_decls: HashMap<Symbol, ValueDecl>,
    type_decls: HashMap<Symbol, TypeDecl>,
    class_decls: HashMap<Symbol, TypeClassDecl>,
    instance_decls: Vec<InstanceDecl>,  // Can't index by name

    // Track where each declaration starts in source
    decls_ref_loc: HashMap<DeclId, SourceSpan>,

    // Import/export info
    imports: Vec<Import>,
    exports: Option<Vec<Export>>,
}
```

## Key Design Decisions

### Why Index by Name?
- Fast lookup for renaming phase
- Enables checking if declaration exists
- Supports incremental updates to single declarations

### Why Relative Spans?
Example:
```purescript
-- Line 1: Add comment here

-- Line 3
foo :: Int        -- Span: 0-10 relative to decl start
foo = 42          -- Span: 11-20 relative to decl start
```

If we add a line at top, absolute spans change but relative spans don't.

### Instance Declarations Challenge
- Can't index by name (instances don't have unique names)
- Multiple instances for same class allowed
- Currently stored in vector (not ideal for lookups)
- Need better indexing strategy

## Incremental Compilation Benefits

### What Gets Invalidated

When you change a module:
1. Parser output invalidated (whole module reparsed)
2. Indexing phase reruns
3. **BUT** if declaration content unchanged:
   - Relative spans are identical
   - Downstream queries (renaming, typechecking) can reuse cached results

### Fine-Grained Tracking
- Each declaration becomes a potential cache boundary
- Later phases can query individual declarations
- Changes to one declaration don't affect others

## Data Flow

```
ParsedModule (AST with absolute spans)
    ↓
[Indexing] - Extract and organize declarations
    ↓
IndexedModule (Declarations by name, relative spans)
    ↓
[Renaming] - Works on individual declarations
```

## Current Limitations

1. **Instance Indexing**: Linear search through instances
2. **Memory Use**: Duplicates some AST data
3. **Span Conversion**: Overhead of converting between relative/absolute

## Requirements for Completion

### Must Have
1. **Instance Indexing**: Better strategy for finding instances
   - Index by class name?
   - Index by type being instantiated?
   - Bloom filters for quick rejection?

2. **Validation**: Ensure no duplicate names in same namespace

3. **Benchmarking**: Measure indexing overhead

### Optimization Opportunities

1. **Lazy Indexing**: Only index declarations as needed
2. **Incremental Updates**: Update index for changed declarations only
3. **Share More Data**: Reduce AST duplication

## Design Questions

1. **Instance Resolution Strategy**:
   - When do we build instance resolution tables?
   - How to make instance lookup incremental-friendly?
   - Global coherence checking?

2. **Declaration Dependencies**:
   - Track which declarations reference which others?
   - Build dependency graph here or later?

3. **Memory vs Speed**:
   - Is duplicating AST nodes worth the speed gain?
   - Could we use handles/indices instead?

## Testing Considerations

- Ensure relative spans correctly maintained
- Test with files containing thousands of declarations
- Verify declaration lookup performance
- Test duplicate name detection