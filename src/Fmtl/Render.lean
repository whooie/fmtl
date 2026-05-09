import Fmtl.Spec

/-!
Helper functions for implementing `fmt` methods.

These are analogous to Rust's `Formatter::pad` and `Formatter::pad_integral` —
reusable building blocks that handle width, fill, alignment, zero-padding,
sign, and alternate-mode prefixes so that individual `fmt` implementations
don't have to reimplement this logic.
-/

namespace Fmtl

/--
Pad `s` to at least `width` characters using `fill` and `align`. Returns `s`
unchanged if it is already wide enough.
-/
def padString
    (s : String)
    (width : Nat)
    (fill : Char)
    (align : Alignment) :
    String :=
  let len := s.length
  if len >= width then s
  else
    let total := width - len
    let fillStr := fun n => String.ofList (List.replicate n fill)
    match align with
    | .left   => s ++ fillStr total
    | .right  => fillStr total ++ s
    | .center =>
      let l := total / 2
      fillStr l ++ s ++ fillStr (total - l)

/--
Sign-aware zero-padding. Inserts `'0'`s between
`signPrefix` (e.g. `"-0x"`) and `digits` (e.g. `"ff"`) to
reach `width`.
-/
def zeroPad (signPrefix : String) (digits : String) (width : Nat) : String :=
  let filled := signPrefix.length + digits.length
  if filled >= width then signPrefix ++ digits
  else
    signPrefix
      ++ String.ofList (List.replicate (width - filled) '0')
      ++ digits

/--
Apply the full `FormatSpec` to already-rendered core content.

- If `zeroPad` is set, delegates to `zeroPad` for sign-aware zero insertion.
- Otherwise, concatenates `signPrefix ++ body` and pads with `padString`.

`signPrefix` should contain any combination of sign character and alternate
prefix (e.g. `"+0x"`, `"-"`, `""`). Build it with `numPrefix`. `body` is the
main formatted content (digits, mantissa, etc.) without sign or prefix.
-/
def applySpec
    (spec : FormatSpec)
    (signPrefix : String)
    (body : String) :
    String :=
  match spec.width with
  | none => signPrefix ++ body
  | some w =>
    if spec.zeroPad then
      zeroPad signPrefix body w
    else
      let align := spec.align.getD .left
      padString (signPrefix ++ body) w spec.fill align

/--
Build the sign + alternate-prefix string for a numeric value.

Combines the sign character (from `spec.sign` and `neg`) with the
alternate-mode prefix (from `spec.alternate` and `kind`). For example,
`numPrefix spec .lowerHex true` with `spec.alternate = true` returns `"-0x"`.
-/
def numPrefix (spec : FormatSpec) (kind : FormatKind) (neg : Bool) : String :=
  let sign := if neg then "-"
    else match spec.sign with
      | .plus  => "+"
      | .minus => ""
  let alt := if spec.alternate then
    match kind with
    | .binary   => "0b"
    | .octal    => "0o"
    | .lowerHex => "0x"
    | .upperHex => "0X"
    | _         => ""
  else ""
  sign ++ alt

end Fmtl
