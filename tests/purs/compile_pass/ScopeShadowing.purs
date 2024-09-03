-- @shouldWarnWith ScopeShadowing
-- FIXME: Scope shadowing doesn't works properly
module Main where

import Prelude as P

-- No warning at the definition, only when the name is later resolved
data Unit = Unit

-- This is only a warning as the `Prelude` import is implicit. If `Unit` was
-- named explicitly in an import list, then this reference to `Unit`
-- would be a `ScopeConflict` error instead.
test :: Unit
test = P.const Unit P.unit
