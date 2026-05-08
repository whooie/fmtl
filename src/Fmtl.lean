import Fmtl.Spec
import Fmtl.Parse
import Fmtl.Class
import Fmtl.Render
import Fmtl.Printf
import Fmtl.Instances

/-!
# fmtl — Rust-style string formatting for Lean 4

A type-safe formatting library with compile-time format string
parsing. Import this module and `open Fmtl` to use `printf`:

```lean
import Fmtl
open Fmtl

#eval printf "hello {}, {:08x}!" "world" (255 : Nat)
-- "hello world, 000000ff!"
```

See `Fmtl.Class` for the type class hierarchy and instructions
on adding formatting support for custom types.
-/
