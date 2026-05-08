import Fmtl.Spec

/-!
Formatting type classes and the `FmtArg` coercion bridge.

Each type class corresponds to one `FormatKind` variant and
has a single method `fmt : α -> FormatSpec -> String` that is
responsible for the **complete** formatted output, including
sign, alternate prefix, precision, width, fill, and alignment.
Helper functions in `Fmtl.Render` (e.g. `applySpec`,
`numPrefix`) are available for use within `fmt` implementations.

## Registering a custom type

To make a custom type usable with `printf`, implement the
relevant class and register a `Coe` instance:

```lean
instance : Display MyType where
  fmt x spec := ...
instance : Coe MyType (FmtArg .display) := Display.coe
```
-/

namespace Fmtl

/--
Default formatting via `{}`. Analogous to Rust's
`std::fmt::Display`.
-/
class Display (α : Type) where
  /--
  Produce the fully formatted string for `α`, respecting all
  fields of `spec`.
  -/
  fmt : α -> FormatSpec -> String

/--
Binary formatting via `{:b}`. Analogous to Rust's
`std::fmt::Binary`.
-/
class Binary (α : Type) where
  fmt : α -> FormatSpec -> String

/--
Octal formatting via `{:o}`. Analogous to Rust's
`std::fmt::Octal`.
-/
class Octal (α : Type) where
  fmt : α -> FormatSpec -> String

/--
Lower-case hex formatting via `{:x}`. Analogous to Rust's
`std::fmt::LowerHex`.
-/
class LowerHex (α : Type) where
  fmt : α -> FormatSpec -> String

/--
Upper-case hex formatting via `{:X}`. Analogous to Rust's
`std::fmt::UpperHex`.
-/
class UpperHex (α : Type) where
  fmt : α -> FormatSpec -> String

/--
Lower-case scientific notation via `{:e}`. Analogous to Rust's
`std::fmt::LowerExp`.
-/
class LowerExp (α : Type) where
  fmt : α -> FormatSpec -> String

/--
Upper-case scientific notation via `{:E}`. Analogous to Rust's
`std::fmt::UpperExp`.
-/
class UpperExp (α : Type) where
  fmt : α -> FormatSpec -> String

/--
Type-erased formatting closure, parameterized by `FormatKind`.
The `printf` macro coerces user arguments to `FmtArg kind`
via `Coe` instances, which are created using the `.coe`
helpers below.
-/
structure FmtArg (_ : FormatKind) where
  /--
  Run the captured formatting closure with the given spec.
  -/
  run : FormatSpec -> String

/--
Build a `Coe α (FmtArg .display)` from a `Display` instance.

Usage: `instance : Coe MyType (FmtArg .display) := Display.coe`
-/
@[reducible]
def Display.coe [Display α] : Coe α (FmtArg .display) :=
  ⟨fun x => ⟨Display.fmt x⟩⟩

/--
Build a `Coe α (FmtArg .binary)` from a `Binary` instance.
-/
@[reducible]
def Binary.coe [Binary α] : Coe α (FmtArg .binary) :=
  ⟨fun x => ⟨Binary.fmt x⟩⟩

/--
Build a `Coe α (FmtArg .octal)` from an `Octal` instance.
-/
@[reducible]
def Octal.coe [Octal α] : Coe α (FmtArg .octal) :=
  ⟨fun x => ⟨Octal.fmt x⟩⟩

/--
Build a `Coe α (FmtArg .lowerHex)` from a `LowerHex`
instance.
-/
@[reducible]
def LowerHex.coe [LowerHex α] : Coe α (FmtArg .lowerHex) :=
  ⟨fun x => ⟨LowerHex.fmt x⟩⟩

/--
Build a `Coe α (FmtArg .upperHex)` from an `UpperHex`
instance.
-/
@[reducible]
def UpperHex.coe [UpperHex α] : Coe α (FmtArg .upperHex) :=
  ⟨fun x => ⟨UpperHex.fmt x⟩⟩

/--
Build a `Coe α (FmtArg .lowerExp)` from a `LowerExp`
instance.
-/
@[reducible]
def LowerExp.coe [LowerExp α] : Coe α (FmtArg .lowerExp) :=
  ⟨fun x => ⟨LowerExp.fmt x⟩⟩

/--
Build a `Coe α (FmtArg .upperExp)` from an `UpperExp`
instance.
-/
@[reducible]
def UpperExp.coe [UpperExp α] : Coe α (FmtArg .upperExp) :=
  ⟨fun x => ⟨UpperExp.fmt x⟩⟩

/--
Build a `ToString` instance from a `Display` instance.
Calls `Display.fmt` with a default `FormatSpec`, so the
resulting string matches the output of `printf "{}" x`.

Usage:
`instance : ToString MyType := Display.toToString`
-/
@[reducible]
def Display.toToString [Display α] : ToString α :=
  ⟨fun x => Display.fmt x {}⟩

end Fmtl
