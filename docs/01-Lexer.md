# Lexer

## Overview
The lexer converts raw source text into a stream of tokens. In purs-oxide, this is a handwritten lexer (not generated) due to PureScript's complex layout rules.

## Key Challenges

### Layout Sensitivity
PureScript uses indentation-based syntax (like Haskell). The lexer must:
- Track indentation levels
- Insert virtual braces `{}` and semicolons `;` based on layout
- Handle explicit braces that override layout rules
- Support offside rule for `where`, `let`, `do`, `case` blocks

### Current State
- ✅ Basic tokenization works
- ✅ Layout handling implemented (passes original compiler's layout tests)
- ⚠️ Tokens store `String` instead of slices (performance issue - causes allocations)

## Incremental Compilation Considerations

### Why Lexer is NOT Incremental
- Lexing happens at module granularity (entire file at once)
- This is intentional - layout rules require seeing the whole file
- Making lexer incremental would be complex and likely not worth it

### Impact on Incremental Compilation
- When any character in a file changes, entire file is re-lexed
- This is acceptable because lexing is fast relative to later stages
- Tokens could cache better if they stored spans/slices instead of owned strings

## Token Representation

Current:
```rust
struct Token {
    kind: TokenKind,
    value: String,  // Problem: allocates for each token
    span: SourceSpan,
}
```

Potential improvement:
```rust
struct Token {
    kind: TokenKind,
    span: SourceSpan,  // Just store position, look up text when needed
}
```

## Data Flow

```
Source Text (String)
    ↓
[Lexer] - Whole module at once
    ↓
Vec<Token> - Stream of tokens with layout tokens inserted
    ↓
[Parser]
```

## Open Questions

1. **Token Storage**: Should we refactor to use spans instead of owned strings?
   - Pro: Less allocation, tokens become `Copy`
   - Con: Need source text available when accessing token values

2. **Error Recovery**: How should lexer handle invalid characters?
   - Current: Returns error tokens
   - Alternative: Skip and continue (better for IDE experience)

3. **Unicode**: Full Unicode identifier support?
   - Current: Partial support
   - Needed: Complete Unicode general categories

## Requirements for Completion

1. **Performance**: Make tokens use spans/slices instead of String
2. **Error Recovery**: Improve to support better IDE experience
3. **Benchmarking**: Measure lexing time for large files

## Testing Strategy

- Test against original compiler's layout test suite
- Ensure all valid PureScript code can be lexed
- Property testing for layout rule edge cases