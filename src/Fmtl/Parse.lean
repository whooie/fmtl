import Fmtl.Spec

/-!
Format string parser.

Converts a format string like `"hello {:>10.3x}"` into a
`List Directive`. All definitions are `@[reducible]` so that
they can be called at compile time by the `printf` macro.
-/

namespace Fmtl

@[reducible]
private def isAlignChar (c : Char) : Bool :=
  c == '<' || c == '^' || c == '>'

@[reducible]
private def toAlignment (c : Char) : Alignment :=
  if c == '<' then .left
  else if c == '^' then .center
  else .right

@[reducible]
private def parseFormatKind (c : Char) : Option FormatKind :=
  match c with
  | 'b' => some .binary
  | 'o' => some .octal
  | 'x' => some .lowerHex
  | 'X' => some .upperHex
  | 'e' => some .lowerExp
  | 'E' => some .upperExp
  | _   => none

@[reducible]
private def isDigitChar (c : Char) : Bool :=
  c.toNat >= '0'.toNat && c.toNat <= '9'.toNat

@[reducible]
private def digitVal (c : Char) : Nat :=
  c.toNat - '0'.toNat

@[reducible]
private def parseDigitsAux
    (acc : Nat) (found : Bool) :
    List Char -> Option Nat × List Char
  | c :: cs =>
    if isDigitChar c
    then parseDigitsAux (acc * 10 + digitVal c) true cs
    else (if found then some acc else none, c :: cs)
  | [] => (if found then some acc else none, [])

@[reducible]
private def parseDigits
    (chars : List Char) : Option Nat × List Char :=
  parseDigitsAux 0 false chars

@[reducible]
private def takeSpec : List Char -> List Char -> List Char × List Char
  | '}' :: rest, acc => (acc.reverse, rest)
  | c :: rest,   acc => takeSpec rest (c :: acc)
  | [],          acc => (acc.reverse, [])

/--
Parse the interior of a `{:...}` specifier (the part after
the colon) into a `FormatSpec` and `FormatKind`.

Grammar:

```
[[fill]align][sign][#][0][width][.precision][type]
```
-/
@[reducible]
def parseInner (chars : List Char) : FormatSpec × FormatKind :=
  -- fill + alignment
  let (fill, align, rest) := match chars with
    | c1 :: c2 :: cs =>
      if isAlignChar c2 then
        (c1, some (toAlignment c2), cs)
      else if isAlignChar c1 then
        (' ', some (toAlignment c1), c2 :: cs)
      else (' ', none, chars)
    | [c1] =>
      if isAlignChar c1 then
        (' ', some (toAlignment c1), [])
      else (' ', none, chars)
    | [] => (' ', none, [])
  -- sign
  let (sign, rest) := match rest with
    | '+' :: cs => (Sign.plus, cs)
    | '-' :: cs => (Sign.minus, cs)
    | _ => (.minus, rest)
  -- alternate '#'
  let (alt, rest) := match rest with
    | '#' :: cs => (true, cs)
    | _ => (false, rest)
  -- zero-pad '0'
  let (zp, rest) := match rest with
    | '0' :: cs => (true, cs)
    | _ => (false, rest)
  -- width
  let (width, rest) := parseDigits rest
  -- '.' precision
  let (prec, rest) := match rest with
    | '.' :: cs =>
      let (n, cs') := parseDigits cs
      (some (n.getD 0), cs')
    | _ => (none, rest)
  -- type character
  let kind := match rest with
    | [c] => (parseFormatKind c).getD .display
    | _   => .display
  let spec := {
    fill,
    align,
    sign,
    alternate := alt,
    zeroPad := zp,
    width,
    precision := prec,
  }
  (spec, kind)

@[reducible]
private def parseAux : Nat -> List Char -> List Char -> List Directive
  | 0, _, acc =>
    if acc.isEmpty then []
    else [.literal (String.ofList acc.reverse)]
  | _, [], acc =>
    if acc.isEmpty then []
    else [.literal (String.ofList acc.reverse)]
  | fuel + 1, '{' :: '{' :: rest, acc =>
    parseAux fuel rest ('{' :: acc)
  | fuel + 1, '}' :: '}' :: rest, acc =>
    parseAux fuel rest ('}' :: acc)
  | fuel + 1, '{' :: rest, acc =>
    let lit :=
      if acc.isEmpty then
        []
      else
        [Directive.literal (String.ofList acc.reverse)]
    let (specChars, remaining) := takeSpec rest []
    let innerChars := match specChars with
      | ':' :: cs => cs
      | _ => []
    let (spec, kind) := parseInner innerChars
    lit
      ++ [.placeholder spec kind]
      ++ parseAux fuel remaining []
  | fuel + 1, c :: rest, acc =>
    parseAux fuel rest (c :: acc)

/--
Parse a format string into a list of `Directive`s.

Handles `{}` placeholders, `{:spec}` specifiers, and
`{{` / `}}` escape sequences for literal braces.

Called at compile time by the `printf` macro.
-/
@[reducible]
def parse (fmt : String) : List Directive :=
  parseAux fmt.toList.length fmt.toList []

end Fmtl
