[ ] SourceSpans relative to declaration
 
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

[ ] error reporting

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

[ ] desugaring
[ ] operator precedence parsing
[ ] SCC for real
[ ] kind checking

[ ] codegen
[ ] salsa persistence
[ ] `type_of`
[ ] handle shadowing in local context (removing stuff from local context when it goes out of scope)

[ ] clean up pipeline/passes/queries
[ ] Avoid interning QualifiedName and AbsoluteName
[ ] performance metrics/tracing
