# Incremental Compilation Overview

## The Problem with Original PureScript Compiler
The original compiler attempts incremental compilation but fails to deliver:
- Coarse-grained caching (whole module level)
- Poor change detection
- Invalidates too much on small changes
- Not truly incremental in practice

## Our Solution: Salsa-Based Architecture
Inspired by Rust's compiler architecture, using [Salsa](https://github.com/salsa-rs/salsa) - a framework for incremental computation.

## Core Principles

### 1. Query-Based Architecture
Instead of a pipeline, compilation is a graph of queries:
```
Source File → Parse → Index → Rename → Type Check → Code Gen
                ↑       ↑       ↑         ↑            ↑
              Cached  Cached  Cached   Cached      Cached
```

### 2. Fine-Grained Invalidation
Change detection at multiple levels:
- **Module level**: Source file changes
- **Declaration level**: Individual declarations
- **SCC level**: Mutually recursive groups
- **Type level**: Signatures vs implementations

### 3. Automatic Dependency Tracking
Salsa automatically tracks what each query depends on:
- No manual dependency management
- Precise invalidation
- Optimal recompilation

## The Incremental Flow

### Example: Changing One Function

```purescript
-- Module Foo
foo :: Int -> Int
foo x = x + 1     -- Change to: x + 2

bar :: Int -> Int
bar x = foo x * 2
```

What happens:
1. **Source Changed**: `Foo.purs` modified
2. **Parse**: Entire module reparsed (necessary due to layout)
3. **Index**: Re-index module (fast)
4. **Rename**:
   - `foo` renamed (changed)
   - `bar` renamed (hash matches cache, reused!)
5. **Type Check**:
   - `foo` type-checked (implementation changed)
   - `bar` signature unchanged, cache hit!
6. **Code Gen**:
   - `foo` regenerated
   - `bar` code reused from cache

### Example: Changing Type Signature

```purescript
foo :: Int -> Int    -- Change to: Int -> String
foo x = show x
```

What invalidates:
1. All callers of `foo` must re-type-check
2. Their code must regenerate
3. But unchanged functions remain cached

## Query Dependency Graph

```
ModuleSource(id)
    ↓
parsed_module(id) ←─────────────┐
    ↓                           │ (re-parse on any change)
indexed_module(id)              │
    ↓                           │
renamed_module(id) ──┐          │
    ↓                │          │
    ├─→ imported_decls(id) ←────┤ (cache imports separately)
    ├─→ exported_decls(id) ←────┤ (cache exports separately)
    └─→ scc_of(decl_id)        │
            ↓                   │
        typechecked_scc(id) ←──┘ (only re-check affected SCCs)
            ↓
        scc_code(id) (cached per SCC)
```

## Granularity Decisions

### Module Level (Coarse)
- **Parsing**: Whole module (due to layout rules)
- **Indexing**: Whole module (need all declarations)

### Declaration Level (Fine)
- **Renaming**: Could be finer but SCC computation needs all
- **Type inference**: Would be per-declaration if not for mutual recursion

### SCC Level (Optimal)
- **Type checking**: Perfect granularity (mutual recursion groups)
- **Code generation**: Per SCC for good caching

## Cache Persistence

### In-Memory (Current)
- Fast during single compilation session
- Lost between runs
- Good for development with file watcher

### Persistent (Decided Approach)
Use Salsa's persistence support with careful implementation:

#### Cache Location
Store in `.purs-oxide-cache/` directory:
- Project-local cache
- Easy to gitignore
- Easy to clean when needed

#### Implementation Strategy
```rust
// Cache metadata for validation
struct CacheMetadata {
    compiler_version: String,
    file_hashes: HashMap<PathBuf, u64>,
    timestamp: SystemTime,
}

// Save cache to disk
fn save_cache(db: &Database) -> Result<()> {
    let cache_dir = Path::new(".purs-oxide-cache");
    fs::create_dir_all(cache_dir)?;

    // Save Salsa state
    db.save_to(cache_dir.join("salsa.db"))?;

    // Save metadata for validation
    save_metadata(cache_dir.join("metadata.json"))?;
    Ok(())
}

// Load and validate cache
fn load_cache() -> Result<Database> {
    let cache_dir = Path::new(".purs-oxide-cache");

    // Check metadata first
    if !validate_cache(cache_dir.join("metadata.json"))? {
        return Err("Cache invalid");
    }

    // Load Salsa state
    Database::load_from(cache_dir.join("salsa.db"))
}
```

#### Salsa Version Consideration
- Currently using fork: `branch = "derive-debug-withdb"`
- **TODO**: Investigate Salsa upgrade effort during implementation
- Don't block on upgrade, but evaluate if we hit limitations

Benefits:
- Instant startup on unchanged code
- Incremental across sessions
- Clear cache invalidation rules

## Performance Characteristics

### Best Case (Small Change)
- Parse: ~10ms (one module)
- Rename: <1ms (mostly cached)
- Type check: <5ms (one SCC)
- Code gen: <1ms (one SCC)
- **Total: ~15ms**

### Worst Case (Type Signature Change)
- Parse: ~10ms (one module)
- Rename: ~5ms (module + dependents)
- Type check: ~50ms (affected SCCs)
- Code gen: ~10ms (affected SCCs)
- **Total: ~75ms**

### Original Compiler Comparison
- Original: 2-5 seconds for any change
- Ours: 15-75ms for typical changes
- **Speedup: 25-300x**

## Challenges and Solutions

### Challenge 1: Layout Rules
**Problem**: Can't parse incrementally due to indentation
**Solution**: Parse whole module, but make spans relative

### Challenge 2: Mutual Recursion
**Problem**: Must type-check related declarations together
**Solution**: SCC computation groups them optimally

### Challenge 3: Instance Resolution
**Problem**: Global coherence checking expensive
**Solution**: Index instances by module, cache resolutions

### Challenge 4: FFI Integration
**Problem**: External JS files not tracked
**Solution**: Include FFI hash in module source

## Optimization Opportunities

### 1. Parallel Compilation
Salsa supports parallel queries:
```rust
db.module_ids()
  .par_iter()  // Parallel iteration
  .for_each(|id| typecheck_module(db, id));
```

### 2. Speculative Compilation
Start compiling before save:
- Track unsaved changes
- Compile speculatively
- Commit cache on save

### 3. Incremental Parsing
Future: Tree-sitter style incremental parsing
- Parse only changed region
- Update AST incrementally
- Requires different parser architecture

## Success Metrics

### Must Have
- ✅ Sub-second incremental compilation
- ✅ Correct caching (no stale results)
- ⬜ Persistent cache
- ⬜ Parallel compilation

### Nice to Have
- ⬜ Sub-10ms response for IDE queries
- ⬜ Distributed cache sharing
- ⬜ Incremental parsing
- ⬜ Speculative compilation

## Comparison with Other Compilers

### Rust (rustc)
- Similar Salsa-based approach
- Our inspiration
- More complex due to borrow checker

### TypeScript (tsc)
- File-based incremental
- Less fine-grained than ours
- No query system

### Haskell (GHC)
- Module-level incremental
- Recompilation avoidance
- Not as fine-grained as Salsa

## Testing Incremental Behavior

### Correctness
- Same output as full compilation
- No stale cache bugs
- Proper invalidation

### Performance
- Measure cache hit rates
- Track recompilation scope
- Benchmark against original

### Scenarios to Test
1. Change implementation (keep signature)
2. Change signature
3. Add new declaration
4. Change imports
5. Modify FFI
6. Parallel compilation race conditions