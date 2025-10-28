# Type Checking

## Overview
Type checking validates that the program is type-safe and infers types where not explicitly provided. PureScript uses Hindley-Milner type inference with extensions for type classes, rows, and higher-kinded types.

## Current Status
⚠️ **VERY INCOMPLETE** - Basic structure exists but most functionality missing

## Type System Features

### Core Hindley-Milner
- Type inference for let-polymorphism
- Parametric polymorphism
- Type unification

### PureScript Extensions
1. **Type Classes**: Ad-hoc polymorphism
2. **Row Polymorphism**: Extensible records and variants
3. **Higher-Kinded Types**: Types parameterized by type constructors
4. **Rank-N Types**: Higher-rank polymorphism
5. **Functional Dependencies**: Type-level functions
6. **Type-Level Programming**: Kind system, type operators

## Incremental Architecture

### Query Structure
```rust
// Type check a whole module
typecheck_module(db, module_id) -> ()  // Uses accumulator for errors

// Type check an SCC (mutually recursive group)
typechecked_scc(db, scc_id) -> HashMap<DeclId, (Expr, Type)>

// Get type of a declaration (for use by other SCCs)
type_of_value(db, decl_id) -> Type
```

### SCC-Based Type Checking
Why SCCs are the unit of type checking:
```purescript
-- Must infer together (mutual recursion)
foo :: _           -- Type to be inferred
foo x = bar (x + 1)

bar :: _           -- Type to be inferred
bar y = foo (y * 2)
```

### Incremental Benefits
1. **Type Signature Changes**: Only affected SCCs re-checked
2. **Implementation Changes**: If signature unchanged, dependents not re-checked
3. **Parallel Type Checking**: Independent SCCs can be checked in parallel

## Type Inference Algorithm

### Basic Flow
1. **Collect Constraints**: Walk expression tree generating type constraints
2. **Solve Constraints**: Unification algorithm
3. **Generalize**: Introduce type variables for polymorphism
4. **Instantiate**: Replace type variables when used

### Constraint Generation
```rust
// Pseudo-code for constraint generation
fn infer(expr: Expr, env: TypeEnv) -> (Type, Constraints) {
    match expr {
        Literal(Int(_)) => (Int, no_constraints),
        Variable(name) => instantiate(env.lookup(name)),
        Lambda(param, body) => {
            let param_ty = fresh_type_var();
            let (body_ty, cs) = infer(body, env.extend(param, param_ty));
            (Function(param_ty, body_ty), cs)
        }
        Application(fun, arg) => {
            let (fun_ty, cs1) = infer(fun, env);
            let (arg_ty, cs2) = infer(arg, env);
            let result_ty = fresh_type_var();
            let cs3 = unify(fun_ty, Function(arg_ty, result_ty));
            (result_ty, cs1 + cs2 + cs3)
        }
        // ... more cases
    }
}
```

## Type Classes

### Instance Resolution
Need incremental-friendly approach:
```rust
// Index instances by class and module
instances_in_module(db, class_id, module_id) -> Vec<Instance>

// Resolution algorithm:
// 1. Find all modules that could have instances
// 2. Query instances from those modules
// 3. Check for matching instance
// 4. Cache resolution result
```

### Coherence Checking
- No overlapping instances
- Orphan rule (instance must be in same module as class or type)
- Functional dependency validation

### Dictionary Passing
Type classes compile to dictionary passing:
```purescript
-- Source
show :: Show a => a -> String

-- After type checking (conceptually)
show :: ShowDict a -> a -> String
```

### Method-Level Instance Tracking (Decided)
Track which methods of each instance are actually used for optimal code generation:

```rust
#[salsa::tracked]
pub struct UsedInstance {
    class: DeclId,
    type_: Type,
    used_methods: HashSet<String>,  // Track at method granularity
}
```

**Benefits:**
- Only generate code for methods that are actually used
- Significant bundle size reduction
- Example: If only `eq` from `Eq` class is used, don't generate `neq`

**Implementation:**
1. During bundling/tree-shaking, track method usage
2. Generate instance dictionaries with only used methods
3. Cache based on (instance, used_methods) pair

```purescript
-- If only 'eq' is used in the program
class Eq a where
  eq :: a -> a -> Boolean
  neq :: a -> a -> Boolean  -- Not used, won't be in bundle

-- Generated JavaScript
const Eq$Foo = {
  eq: (x) => (y) => ...  // Only this method
  // neq not included
};
```

## Row Polymorphism

### Record Types
```purescript
type Person r = { name :: String, age :: Int | r }

-- Can extend:
type Employee = Person ( salary :: Number )
```

### Challenges
1. **Row Unification**: More complex than simple type unification
2. **Duplicate Labels**: Detect and error
3. **Row Kinds**: Proper kind checking for rows

## Current Implementation Gaps

### Critical Missing
1. **Constraint Solver**: Core unification algorithm
2. **Type Class Resolution**: Instance lookup and coherence
3. **Row Unification**: For records and variants
4. **Generalization**: Let-polymorphism
5. **Kind Checking**: Validate type-level applications

### Important Missing
1. **Higher-Rank Types**: ForAll in negative positions
2. **Type Operators**: Infix type constructors
3. **Functional Dependencies**: Type-level computation
4. **Pattern Matching**: Exhaustiveness checking
5. **Do Notation**: Monad constraint generation

## Error Reporting

### Requirements for LLM-Friendly Errors
```rust
struct TypeError {
    location: SourceSpan,
    expected: Type,
    actual: Type,
    context: TypeContext,  // Surrounding expression
    suggestions: Vec<Fix>,  // Possible fixes
}
```

### Error Types
1. **Unification Errors**: Type mismatch
2. **Undefined Names**: After renaming (shouldn't happen)
3. **Instance Errors**: No instance found
4. **Kind Errors**: Type-level application mistakes
5. **Exhaustiveness**: Missing pattern cases

## Performance Considerations

### Bottlenecks
1. **Instance Resolution**: Can be expensive with many instances
2. **Row Unification**: Complex algorithm
3. **Constraint Solving**: Large constraint sets

### Optimization Opportunities
1. **Cache Instance Resolution**: Key by (constraint, available_modules)
2. **Parallel SCC Checking**: Check independent SCCs concurrently
3. **Early Cutoff**: Stop if explicit signature matches inferred

## Requirements for Completion

### Phase 1: Basic Type Checking
1. Simple type inference (no type classes)
2. Function types
3. Basic data types
4. Let polymorphism

### Phase 2: Type Classes
1. Instance resolution
2. Constraint solving
3. Dictionary transformation
4. Coherence checking

### Phase 3: Advanced Features
1. Row polymorphism
2. Higher-kinded types
3. Rank-N types
4. Type operators

## Design Questions

1. **Constraint Representation**:
   - Immediate solving or collect all first?
   - How to represent for good error messages?

2. **Instance Caching**:
   - Cache at what granularity?
   - How to invalidate efficiently?

3. **Type Representation**:
   - Unified representation or phase-specific?
   - How to handle type aliases?

4. **Error Recovery**:
   - Continue checking after errors?
   - Provide partial type information?

## Testing Strategy

- Port type checking tests from original compiler
- Property-based testing for inference
- Test incremental behavior (change detection)
- Benchmark against original compiler