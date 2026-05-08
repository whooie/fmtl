# fmtl

Rust-style string formatting for Lean 4.

`fmtl` provides a `printf` macro that parses format strings at compile
time, catching argument count mismatches before your code ever runs.
Placeholders use Rust's brace syntax and map to a hierarchy of
formatting type classes.

## Usage

```lean
import Fmtl
open Fmtl

#eval printf "hello {}!" "world"
-- "hello world!"

#eval printf "{:>10}" "right"
-- "     right"

#eval printf "{:08x}" (255 : Nat)
-- "000000ff"

#eval printf "{:+.2}" (-3.14 : Float)
-- "-3.14"

#eval printf "{:#010x}" (48879 : Nat)
-- "0x0000beef"

#eval printf "{:^7}" "hi"
-- "  hi   "
```

## Format spec

```
{[:[fill][align][sign][#][0][width][.precision][type]]}
```

| Feature     | Syntax                  | Example  | Result         |
|-------------|-------------------------|----------|----------------|
| Alignment   | `<` `^` `>`             | `{:>8}`  | `"   hello"`   |
| Fill char   | any char before align   | `{:_>8}` | `"___hello"`   |
| Sign        | `+` or `-`              | `{:+}`   | `"+42"`        |
| Alternate   | `#`                     | `{:#x}`  | `"0xff"`       |
| Zero-pad    | `0`                     | `{:08x}` | `"000000ff"`   |
| Width       | integer                 | `{:10}`  | `"hello     "` |
| Precision   | `.N`                    | `{:.3}`  | `"3.140"`      |
| Type        | `b` `o` `x` `X` `e` `E` | `{:b}`   | `"11111111"`   |

## Type classes

Each format type maps to a class:

| Placeholder | Class      |
|-------------|------------|
| `{}`        | `Display`  |
| `{:b}`      | `Binary`   |
| `{:o}`      | `Octal`    |
| `{:x}`      | `LowerHex` |
| `{:X}`      | `UpperHex` |
| `{:e}`      | `LowerExp` |
| `{:E}`      | `UpperExp` |

Built-in instances cover `String`, `Char`, `Bool`, `Nat`, `Int*`, `Float`,
`Float32`, and `UInt*`.

## Custom types

To make your own type work with `printf`, implement a formatting class and
register a `Coe` instance. The `Coe` bridge is what lets the `printf` macro
automatically convert your value into the internal `FmtArg` wrapper--without it,
the macro expansion won't type-check.

```lean
import Fmtl
open Fmtl

structure Point where
  x : Int
  y : Int

instance : Display Point where
  fmt p spec :=
    let body := s!"({p.x}, {p.y})"
    applySpec spec "" body

instance : Coe Point (FmtArg .display) := Display.coe

#eval printf "origin: {}" (Point.mk 0 0)
-- "origin: (0, 0)"
```

Each formatting class (e.g. `Display`, `LowerHex`) has a corresponding
`.coe` helper that builds the `Coe` instance for you. Register one per
class your type supports:

```lean
instance : Coe MyType (FmtArg .display)  := Display.coe
instance : Coe MyType (FmtArg .lowerHex) := LowerHex.coe
```

## AI use disclosure

I'm not yet extremely familiar with Lean, but I thought the ecosystem could
really use a formatter (for example, `toString` on a `Float` will always render
the number with `{:.6}`--obviously not good). Hence, the vast majority of code
in this repository was written with AI. Merge requests with improvements are
welcome.
