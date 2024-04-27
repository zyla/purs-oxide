## How to deal with FFI?

### File input

Add a field to ModuleSource - `ffi_source`. Adding a file to Database also requires reading the corresponding FFI file.

### How to bundle

We can't just paste FFI code into the bundle, because there can be name collisions. Before ES modules we could work around that using IIFE, but now we can't (`import`/`export` is only toplevel).

We have several options:

1. Parse the code and perform renaming (note: requires knowing essentially all JS syntax)
2. Find a library that does it (possibly some bundler? swc?)
3. Ignore the issue and emit FFI modules and separate files (just copy the source). Then output of our "bundler" still has to be passed through another bundler if you want to run in browser - but this is already the case if you import external libraries, so I guess it's fine.

TODO: maybe "bundle module" should be named differently? What we want is "compile only what you need".
