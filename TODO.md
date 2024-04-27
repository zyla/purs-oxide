- [x] SourceSpans relative to declaration
 
```
 #[derive(Eq, PartialEq, Debug, Hash, Clone, Copy, DebugWithDb)]
 pub struct SourceSpan {
+    pub decl: Option<AbsoluteName>,
     pub start: usize,
     pub end: usize,
 }
@@ -20,6 +21,15 @@ impl SourceSpan {
     pub fn todo() -> Self {
         Self::unknown()
     }
+
+    pub fn to_file_and_location(&self, db: &dyn crate::Db) -> (String, usize, usize) {
+        let (filename, start_in_file) = decl_absolute_location(db, self.decl);
+        (
+            filename,
+            self.start + start_in_file,
+            self.end + start_in_file,
+        )
+    }
 }


```

- [x] error reporting

```
#[derive(Eq, PartialEq, Debug, Hash, Clone, Copy, DebugWithDb)]
pub struct SourceSpan {
    pub decl: SpanDeclRef,
    pub start: usize,
    pub end: usize,
}

enum SpanDeclRef {
    Module(ModuleId),
    Decl(AbsoluteName),
}
```

- [ ] desugaring
- [ ] operator precedence parsing
- [ ] SCC for real

- [x] codegen
- [ ] salsa persistence
- [ ] `type_of`
- [ ] handle shadowing in local context (removing stuff from local context when it goes out of scope)

- [ ] clean up pipeline/passes/queries
- [ ] Avoid interning QualifiedName and AbsoluteName
- [ ] performance metrics/tracing
- [ ] kind checking

Code generation:

```
// Foo/index.js
export const foo = () => 'xd';

// Bar/index.js
import * as Foo from '../Foo/index.js';

export const bar = Foo.foo;
```

```
// bundle.js
const Foo_foo = () => 'xd';

const Bar_bar = Foo_foo;

Bar_bar();
```

```
// bundle.js
const Foo = {};
Foo.foo = () => 'xd';

const bar = Foo.foo;
```

// FunctionType(Vec<Type>, Box<Type>)
// [a b] -> c
// a -> b -> c

- [ ] incremental-friendly instance resolution
- [x] Prim module (and autoimport it)
- [ ] Bundle CLI command
- [ ] Tests that bundle and run a JS program
- [ ] How to bundle with multiple entrypoints but without duplicates?
