/-!
Core types for format string representation.
-/

namespace Fmtl

/--
Text alignment within a padded field.
-/
inductive Alignment where
  | left
  | center
  | right
  deriving Repr, BEq, Inhabited

/--
Sign display mode for numeric values.

- `plus`: always show `+` or `-`
- `minus`: only show `-` for negative values
-/
inductive Sign where
  | plus
  | minus
  deriving Repr, BEq, Inhabited

/--
Which formatting type class a placeholder requires.
Each variant maps to one of the seven formatting classes
(e.g. `display` -> `Display`, `lowerHex` -> `LowerHex`).
-/
inductive FormatKind where
  | display
  | binary
  | octal
  | lowerHex
  | upperHex
  | lowerExp
  | upperExp
  deriving Repr, BEq, Inhabited

/--
Parsed representation of a single format specifier's options,
corresponding to the grammar:

```
[[fill]align][sign][#][0][width][.precision]
```

All fields have sensible defaults matching Rust's behavior.
-/
structure FormatSpec where
  fill : Char := ' '
  align : Option Alignment := none
  sign : Sign := .minus
  alternate : Bool := false
  zeroPad : Bool := false
  width : Option Nat := none
  precision : Option Nat := none
  deriving Repr, Inhabited

/--
A single element of a parsed format string: either a run of
literal text or a placeholder with its specifier and kind.
-/
inductive Directive where
  | literal : String -> Directive
  | placeholder : FormatSpec -> FormatKind -> Directive
  deriving Repr

end Fmtl
