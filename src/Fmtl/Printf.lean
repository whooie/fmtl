import Fmtl.Spec
import Fmtl.Parse
import Fmtl.Class
import Fmtl.Render
import Fmtl.Instances

/-!
The `printf` macro.

Parses the format string at compile time and expands into a
chain of string concatenations, where each placeholder becomes
a coercion to `FmtArg kind` followed by a call to `.run`.
-/

namespace Fmtl

section Quoting
open Lean

private def qKind : FormatKind -> MacroM (TSyntax `term)
  | .display  => `(FormatKind.display)
  | .binary   => `(FormatKind.binary)
  | .octal    => `(FormatKind.octal)
  | .lowerHex => `(FormatKind.lowerHex)
  | .upperHex => `(FormatKind.upperHex)
  | .lowerExp => `(FormatKind.lowerExp)
  | .upperExp => `(FormatKind.upperExp)

private def qAlign : Alignment -> MacroM (TSyntax `term)
  | .left   => `(Alignment.left)
  | .center => `(Alignment.center)
  | .right  => `(Alignment.right)

private def qSign : Sign -> MacroM (TSyntax `term)
  | .plus  => `(Sign.plus)
  | .minus => `(Sign.minus)

private def qOptNat : Option Nat -> MacroM (TSyntax `term)
  | some n => do `(some $(quote n))
  | none   => `(none)

private def qOptAlign :
    Option Alignment -> MacroM (TSyntax `term)
  | some a => do let q <- qAlign a; `(some $q)
  | none   => `(none)

private def qSpec
    (spec : FormatSpec) : MacroM (TSyntax `term) := do
  let fill := quote spec.fill
  let align <- qOptAlign spec.align
  let sign <- qSign spec.sign
  let alt := quote spec.alternate
  let zp := quote spec.zeroPad
  let width <- qOptNat spec.width
  let prec <- qOptNat spec.precision
  `({ fill := $fill, align := $align, sign := $sign,
      alternate := $alt, zeroPad := $zp,
      width := $width,
      precision := $prec : FormatSpec })

end Quoting

/--
Type-safe string formatting with compile-time parsing.

The format string must be a string literal. Placeholders
use Rust-style brace syntax:

```
printf "hello {}, {:08x}!" name value
```

Each placeholder is matched positionally to the trailing
arguments. The placeholder's format kind determines which
type class (`Display`, `LowerHex`, etc.) the argument must
implement. Argument count mismatches are caught at compile
time by the macro; type mismatches are caught by the
elaborator when the `Coe` to `FmtArg` fails.
-/
scoped syntax "printf" str term:max* : term

open Lean in
scoped macro_rules
  | `(printf $fmt:str $args*) => do
    let fmtStr := fmt.getString
    let dirs := parse fmtStr
    let mut body : TSyntax `term <- `("")
    let mut idx : Nat := 0
    for d in dirs do
      match d with
      | .literal s =>
        let lit : TSyntax `term := quote s
        body <- `($body ++ $lit)
      | .placeholder spec kind =>
        if idx >= args.size then
          let msg :=
            s!"printf: not enough arguments "
              ++ s!"(need at least {idx + 1})"
          Macro.throwError msg
        let arg : TSyntax `term := ⟨args[idx]!⟩
        let specQ <- qSpec spec
        let kindQ <- qKind kind
        body <- `($body ++
          (($arg : Fmtl.FmtArg $kindQ).run $specQ))
        idx := idx + 1
    if idx != args.size then
      let msg :=
        s!"printf: too many arguments "
          ++ s!"(expected {idx}, got {args.size})"
      Macro.throwError msg
    return body

section Examples

example : printf "{}" "hello" = "hello" := rfl
example : printf "{:>10}" "right" = "     right" := rfl
example : printf "{:0<8}" "left" = "left0000" := rfl
example : printf "{:^7}" "hi" = "  hi   " := rfl
example : printf "{:08x}" (255 : Nat) = "000000ff" := rfl
example : printf "{:#x}" (255 : Nat) = "0xff" := rfl
example : printf "{:+}" (42 : Int) = "+42" := rfl
example : printf "{:b}" (10 : Nat) = "1010" := rfl
example : printf "{{escaped}}" = "{escaped}" := rfl
example : printf "{}+{}={}" (1 : Nat) (2 : Nat) (3 : Nat) = "1+2=3" := rfl
example : printf "{}" ([1, 2, 3] : List Nat) = "[1, 2, 3]" := rfl
example : printf "{}" (#[4, 5] : Array Nat) = "[4, 5]" := rfl

end Examples

end Fmtl
