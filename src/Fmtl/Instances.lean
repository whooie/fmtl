import Fmtl.Spec
import Fmtl.Class
import Fmtl.Render

/-!
Built-in formatting instances for standard Lean types.

Each instance handles the full `FormatSpec` — sign, alternate
prefix, precision, width, fill, and alignment — using helper
functions from `Fmtl.Render`.

Provides instances of:

- `Display` for `String`, `Char`, `Bool`, `Nat`, `Int`, `Float`, `Float32`,
  `UInt8`, `UInt16`, `UInt32`, `UInt64`, `USize`, `Int8`, `Int16`, `Int32`,
  `Int64`, `ISize`, `List`, `Array`
- `Binary`, `Octal`, `LowerHex`, `UpperHex` for `Nat`, `Int`, `UInt8`,
  `UInt16`, `UInt32`, `UInt64`, `USize`, `Int8`, `Int16`, `Int32`, `Int64`
  `ISize`,
- `LowerExp`, `UpperExp` for `Float`, `Float32`

Each type class instance is accompanied by a corresponding
`Coe α (FmtArg kind)` instance so that values of these types
can be passed directly to `printf`.
-/

namespace Fmtl

/- Numeric helpers -/

private def lowerHexDigit (n : Nat) : Char :=
  if n < 10 then Char.ofNat (n + '0'.toNat)
  else Char.ofNat (n - 10 + 'a'.toNat)

private def upperHexDigit (n : Nat) : Char :=
  if n < 10 then Char.ofNat (n + '0'.toNat)
  else Char.ofNat (n - 10 + 'A'.toNat)

private def decDigit (n : Nat) : Char :=
  Char.ofNat (n + '0'.toNat)

private def binDigit (n : Nat) : Char :=
  if n == 0 then '0' else '1'

private def natToDigitsAux
    (base : Nat)
    (digitFn : Nat -> Char) :
    Nat -> Nat -> List Char -> String
  | _, 0, acc =>
    String.ofList (if acc.isEmpty then ['0'] else acc)
  | 0, _, acc =>
    String.ofList (if acc.isEmpty then ['0'] else acc)
  | fuel + 1, n, acc =>
    natToDigitsAux base digitFn fuel
      (n / base) (digitFn (n % base) :: acc)

private def natToDigits
    (n : Nat) (base : Nat) (digitFn : Nat -> Char) :
    String :=
  natToDigitsAux base digitFn 64 n []

private def intParts (n : Int) : Bool × Nat :=
  if n < 0 then (true, (-n).toNat) else (false, n.toNat)

private def fmtIntLike
    (spec : FormatSpec)
    (kind : FormatKind)
    (neg : Bool)
    (digits : String) :
    String :=
  let pfx := numPrefix spec kind neg
  applySpec spec pfx digits

/- Float helpers -/

private def formatFixedFloat
    (f : Float) (prec : Nat) : String :=
  if prec == 0 then
    toString (f + 0.5).floor.toUInt64.toNat
  else
    let scale := (10.0 : Float) ^ Float.ofNat prec
    let rounded := (f * scale + 0.5).floor.toUInt64.toNat
    let s := toString rounded
    let s := if s.length <= prec
      then
        String.ofList
          (List.replicate (prec + 1 - s.length) '0')
          ++ s
      else s
    let intLen := s.length - prec
    String.ofList (s.toList.take intLen)
      ++ "."
      ++ String.ofList (s.toList.drop intLen)

private def formatSci
    (f : Float) (prec : Nat) (expChar : Char) :
    String :=
  if f == 0.0 then
    let frac := if prec > 0
      then
        "." ++ String.ofList (List.replicate prec '0')
      else ""
    s!"0{frac}{expChar}+00"
  else
    let abs := f.abs
    let rawExp := Float.log10 abs |>.floor
    let mantissa := abs / ((10.0 : Float) ^ rawExp)
    let (mantissa, rawExp) :=
      if mantissa >= 10.0 then
        (mantissa / 10.0, rawExp + 1.0)
      else if mantissa < 1.0 then
        (mantissa * 10.0, rawExp - 1.0)
      else (mantissa, rawExp)
    let mantStr := formatFixedFloat mantissa prec
    let expNeg := rawExp < 0.0
    let expAbs :=
      if expNeg then (-rawExp).toUInt64.toNat
      else rawExp.toUInt64.toNat
    let expStr := toString expAbs
    let expStr :=
      if expStr.length < 2 then "0" ++ expStr
      else expStr
    let expSign := if expNeg then "-" else "+"
    s!"{mantStr}{expChar}{expSign}{expStr}"

private def fmtFloatLike
    (f : Float)
    (spec : FormatSpec)
    (kind : FormatKind)
    (render : Float -> Nat -> String) :
    String :=
  if f.isNaN then applySpec spec "" "NaN"
  else if f.isInf then
    let pfx := numPrefix spec kind (f < 0.0)
    applySpec spec pfx "inf"
  else
    let neg := f < 0.0
    let prec := spec.precision.getD 6
    let pfx := numPrefix spec kind neg
    applySpec spec pfx (render f.abs prec)

private def formatFixedFloat32
    (f : Float32) (prec : Nat) : String :=
  if prec == 0 then
    toString (f + 0.5).floor.toUInt64.toNat
  else
    let scale := (10.0 : Float32) ^ Float32.ofNat prec
    let rounded := (f * scale + 0.5).floor.toUInt64.toNat
    let s := toString rounded
    let s := if s.length <= prec
      then
        String.ofList
          (List.replicate (prec + 1 - s.length) '0')
          ++ s
      else s
    let intLen := s.length - prec
    String.ofList (s.toList.take intLen)
      ++ "."
      ++ String.ofList (s.toList.drop intLen)

private def formatSci32
    (f : Float32) (prec : Nat) (expChar : Char) :
    String :=
  if f == 0.0 then
    let frac := if prec > 0
      then
        "." ++ String.ofList (List.replicate prec '0')
      else ""
    s!"0{frac}{expChar}+00"
  else
    let abs := f.abs
    let rawExp := Float32.log10 abs |>.floor
    let mantissa := abs / ((10.0 : Float32) ^ rawExp)
    let (mantissa, rawExp) :=
      if mantissa >= 10.0 then
        (mantissa / 10.0, rawExp + 1.0)
      else if mantissa < 1.0 then
        (mantissa * 10.0, rawExp - 1.0)
      else (mantissa, rawExp)
    let mantStr := formatFixedFloat32 mantissa prec
    let expNeg := rawExp < 0.0
    let expAbs :=
      if expNeg then (-rawExp).toUInt64.toNat
      else rawExp.toUInt64.toNat
    let expStr := toString expAbs
    let expStr :=
      if expStr.length < 2 then "0" ++ expStr
      else expStr
    let expSign := if expNeg then "-" else "+"
    s!"{mantStr}{expChar}{expSign}{expStr}"

private def fmtFloat32Like
    (f : Float32)
    (spec : FormatSpec)
    (kind : FormatKind)
    (render : Float32 -> Nat -> String) :
    String :=
  if f.isNaN then applySpec spec "" "NaN"
  else if f.isInf then
    let pfx := numPrefix spec kind (f < 0.0)
    applySpec spec pfx "inf"
  else
    let neg := f < 0.0
    let prec := spec.precision.getD 6
    let pfx := numPrefix spec kind neg
    applySpec spec pfx (render f.abs prec)

/- Collection helper -/

private def fmtListCoe [inst : Coe α (FmtArg .display)]
    (xs : List α) (spec : FormatSpec) : String :=
  let inner := String.intercalate ", "
    (xs.map fun x => (inst.coe x).run {})
  let body := "[" ++ inner ++ "]"
  match spec.width with
  | none => body
  | some w =>
    padString body w spec.fill (spec.align.getD .left)

/- Display -/

instance : Display String where
  fmt s spec :=
    let s := match spec.precision with
      | some p => String.ofList (s.toList.take p)
      | none   => s
    match spec.width with
    | none => s
    | some w =>
      padString s w spec.fill (spec.align.getD .left)

instance : Display Char where
  fmt c spec := Display.fmt (String.ofList [c]) spec

instance : Display Bool where
  fmt b spec :=
    Display.fmt (if b then "true" else "false") spec

instance : Display Nat where
  fmt n spec :=
    fmtIntLike spec .display false (toString n)

instance : Display Int where
  fmt n spec :=
    let (neg, abs) := intParts n
    fmtIntLike spec .display neg (toString abs)

instance : Display Float where
  fmt f spec :=
    fmtFloatLike f spec .display (fun abs prec => formatFixedFloat abs prec)

instance : Display Float32 where
  fmt f spec :=
    fmtFloat32Like f spec .display (fun abs prec => formatFixedFloat32 abs prec)

instance : Display UInt8 where
  fmt n s := Display.fmt n.toNat s
instance : Display UInt16 where
  fmt n s := Display.fmt n.toNat s
instance : Display UInt32 where
  fmt n s := Display.fmt n.toNat s
instance : Display UInt64 where
  fmt n s := Display.fmt n.toNat s
instance : Display USize where
  fmt n s := Display.fmt n.toNat s

instance : Display Int8 where
  fmt n s := Display.fmt n.toInt s
instance : Display Int16 where
  fmt n s := Display.fmt n.toInt s
instance : Display Int32 where
  fmt n s := Display.fmt n.toInt s
instance : Display Int64 where
  fmt n s := Display.fmt n.toInt s
instance : Display ISize where
  fmt n s := Display.fmt n.toInt s

instance : Coe String  (FmtArg .display) := Display.coe
instance : Coe Char    (FmtArg .display) := Display.coe
instance : Coe Bool    (FmtArg .display) := Display.coe
instance : Coe Nat     (FmtArg .display) := Display.coe
instance : Coe Int     (FmtArg .display) := Display.coe
instance : Coe Float   (FmtArg .display) := Display.coe
instance : Coe Float32 (FmtArg .display) := Display.coe
instance : Coe UInt8   (FmtArg .display) := Display.coe
instance : Coe UInt16  (FmtArg .display) := Display.coe
instance : Coe UInt32  (FmtArg .display) := Display.coe
instance : Coe UInt64  (FmtArg .display) := Display.coe
instance : Coe USize   (FmtArg .display) := Display.coe
instance : Coe Int8    (FmtArg .display) := Display.coe
instance : Coe Int16   (FmtArg .display) := Display.coe
instance : Coe Int32   (FmtArg .display) := Display.coe
instance : Coe Int64   (FmtArg .display) := Display.coe
instance : Coe ISize   (FmtArg .display) := Display.coe

instance [inst : Coe α (FmtArg .display)] :
    Coe (List α) (FmtArg .display) :=
  ⟨fun xs => ⟨fun spec => fmtListCoe xs spec⟩⟩

instance [inst : Coe α (FmtArg .display)] :
    Coe (Array α) (FmtArg .display) :=
  ⟨fun xs => ⟨fun spec => fmtListCoe xs.toList spec⟩⟩

/- Binary -/

instance : Binary Nat where
  fmt n spec :=
    fmtIntLike spec .binary false
      (natToDigits n 2 binDigit)

instance : Binary Int where
  fmt n spec :=
    let (neg, abs) := intParts n
    fmtIntLike spec .binary neg
      (natToDigits abs 2 binDigit)

instance : Binary UInt8 where
  fmt n s := Binary.fmt n.toNat s
instance : Binary UInt16 where
  fmt n s := Binary.fmt n.toNat s
instance : Binary UInt32 where
  fmt n s := Binary.fmt n.toNat s
instance : Binary UInt64 where
  fmt n s := Binary.fmt n.toNat s
instance : Binary USize where
  fmt n s := Binary.fmt n.toNat s

instance : Binary Int8 where
  fmt n s := Binary.fmt n.toInt s
instance : Binary Int16 where
  fmt n s := Binary.fmt n.toInt s
instance : Binary Int32 where
  fmt n s := Binary.fmt n.toInt s
instance : Binary Int64 where
  fmt n s := Binary.fmt n.toInt s
instance : Binary ISize where
  fmt n s := Binary.fmt n.toInt s

instance : Coe Nat    (FmtArg .binary) := Binary.coe
instance : Coe Int    (FmtArg .binary) := Binary.coe
instance : Coe UInt8  (FmtArg .binary) := Binary.coe
instance : Coe UInt16 (FmtArg .binary) := Binary.coe
instance : Coe UInt32 (FmtArg .binary) := Binary.coe
instance : Coe UInt64 (FmtArg .binary) := Binary.coe
instance : Coe USize  (FmtArg .binary) := Binary.coe
instance : Coe Int8   (FmtArg .binary) := Binary.coe
instance : Coe Int16  (FmtArg .binary) := Binary.coe
instance : Coe Int32  (FmtArg .binary) := Binary.coe
instance : Coe Int64  (FmtArg .binary) := Binary.coe
instance : Coe ISize  (FmtArg .binary) := Binary.coe

/- Octal -/

instance : Octal Nat where
  fmt n spec :=
    fmtIntLike spec .octal false
      (natToDigits n 8 decDigit)

instance : Octal Int where
  fmt n spec :=
    let (neg, abs) := intParts n
    fmtIntLike spec .octal neg
      (natToDigits abs 8 decDigit)

instance : Octal UInt8 where
  fmt n s := Octal.fmt n.toNat s
instance : Octal UInt16 where
  fmt n s := Octal.fmt n.toNat s
instance : Octal UInt32 where
  fmt n s := Octal.fmt n.toNat s
instance : Octal UInt64 where
  fmt n s := Octal.fmt n.toNat s
instance : Octal USize where
  fmt n s := Octal.fmt n.toNat s

instance : Octal Int8 where
  fmt n s := Octal.fmt n.toInt s
instance : Octal Int16 where
  fmt n s := Octal.fmt n.toInt s
instance : Octal Int32 where
  fmt n s := Octal.fmt n.toInt s
instance : Octal Int64 where
  fmt n s := Octal.fmt n.toInt s
instance : Octal ISize where
  fmt n s := Octal.fmt n.toInt s

instance : Coe Nat    (FmtArg .octal) := Octal.coe
instance : Coe Int    (FmtArg .octal) := Octal.coe
instance : Coe UInt8  (FmtArg .octal) := Octal.coe
instance : Coe UInt16 (FmtArg .octal) := Octal.coe
instance : Coe UInt32 (FmtArg .octal) := Octal.coe
instance : Coe UInt64 (FmtArg .octal) := Octal.coe
instance : Coe Int8   (FmtArg .octal) := Octal.coe
instance : Coe Int16  (FmtArg .octal) := Octal.coe
instance : Coe Int32  (FmtArg .octal) := Octal.coe
instance : Coe Int64  (FmtArg .octal) := Octal.coe

/- LowerHex / UpperHex -/

instance : LowerHex Nat where
  fmt n spec :=
    fmtIntLike spec .lowerHex false
      (natToDigits n 16 lowerHexDigit)

instance : LowerHex Int where
  fmt n spec :=
    let (neg, abs) := intParts n
    fmtIntLike spec .lowerHex neg
      (natToDigits abs 16 lowerHexDigit)

instance : LowerHex UInt8 where
  fmt n s := LowerHex.fmt n.toNat s
instance : LowerHex UInt16 where
  fmt n s := LowerHex.fmt n.toNat s
instance : LowerHex UInt32 where
  fmt n s := LowerHex.fmt n.toNat s
instance : LowerHex UInt64 where
  fmt n s := LowerHex.fmt n.toNat s
instance : LowerHex USize where
  fmt n s := LowerHex.fmt n.toNat s

instance : LowerHex Int8 where
  fmt n s := LowerHex.fmt n.toInt s
instance : LowerHex Int16 where
  fmt n s := LowerHex.fmt n.toInt s
instance : LowerHex Int32 where
  fmt n s := LowerHex.fmt n.toInt s
instance : LowerHex Int64 where
  fmt n s := LowerHex.fmt n.toInt s
instance : LowerHex ISize where
  fmt n s := LowerHex.fmt n.toInt s

instance : Coe Nat    (FmtArg .lowerHex) := LowerHex.coe
instance : Coe Int    (FmtArg .lowerHex) := LowerHex.coe
instance : Coe UInt8  (FmtArg .lowerHex) := LowerHex.coe
instance : Coe UInt16 (FmtArg .lowerHex) := LowerHex.coe
instance : Coe UInt32 (FmtArg .lowerHex) := LowerHex.coe
instance : Coe UInt64 (FmtArg .lowerHex) := LowerHex.coe
instance : Coe Int8   (FmtArg .lowerHex) := LowerHex.coe
instance : Coe Int16  (FmtArg .lowerHex) := LowerHex.coe
instance : Coe Int32  (FmtArg .lowerHex) := LowerHex.coe
instance : Coe Int64  (FmtArg .lowerHex) := LowerHex.coe

instance : UpperHex Nat where
  fmt n spec :=
    fmtIntLike spec .upperHex false
      (natToDigits n 16 upperHexDigit)

instance : UpperHex Int where
  fmt n spec :=
    let (neg, abs) := intParts n
    fmtIntLike spec .upperHex neg
      (natToDigits abs 16 upperHexDigit)

instance : UpperHex UInt8 where
  fmt n s := UpperHex.fmt n.toNat s
instance : UpperHex UInt16 where
  fmt n s := UpperHex.fmt n.toNat s
instance : UpperHex UInt32 where
  fmt n s := UpperHex.fmt n.toNat s
instance : UpperHex UInt64 where
  fmt n s := UpperHex.fmt n.toNat s
instance : UpperHex USize where
  fmt n s := UpperHex.fmt n.toNat s

instance : UpperHex Int8 where
  fmt n s := UpperHex.fmt n.toInt s
instance : UpperHex Int16 where
  fmt n s := UpperHex.fmt n.toInt s
instance : UpperHex Int32 where
  fmt n s := UpperHex.fmt n.toInt s
instance : UpperHex Int64 where
  fmt n s := UpperHex.fmt n.toInt s
instance : UpperHex ISize where
  fmt n s := UpperHex.fmt n.toInt s

instance : Coe Nat    (FmtArg .upperHex) := UpperHex.coe
instance : Coe Int    (FmtArg .upperHex) := UpperHex.coe
instance : Coe UInt8  (FmtArg .upperHex) := UpperHex.coe
instance : Coe UInt16 (FmtArg .upperHex) := UpperHex.coe
instance : Coe UInt32 (FmtArg .upperHex) := UpperHex.coe
instance : Coe UInt64 (FmtArg .upperHex) := UpperHex.coe
instance : Coe Int8   (FmtArg .upperHex) := UpperHex.coe
instance : Coe Int16  (FmtArg .upperHex) := UpperHex.coe
instance : Coe Int32  (FmtArg .upperHex) := UpperHex.coe
instance : Coe Int64  (FmtArg .upperHex) := UpperHex.coe

/- LowerExp / UpperExp -/

instance : LowerExp Float where
  fmt f spec :=
    fmtFloatLike f spec .lowerExp (fun abs prec => formatSci abs prec 'e')

instance : UpperExp Float where
  fmt f spec :=
    fmtFloatLike f spec .upperExp (fun abs prec => formatSci abs prec 'E')

instance : Coe Float (FmtArg .lowerExp) := LowerExp.coe
instance : Coe Float (FmtArg .upperExp) := UpperExp.coe

instance : LowerExp Float32 where
  fmt f spec :=
    fmtFloat32Like f spec .lowerExp (fun abs prec => formatSci32 abs prec 'e')

instance : UpperExp Float32 where
  fmt f spec :=
    fmtFloat32Like f spec .upperExp (fun abs prec => formatSci32 abs prec 'E')

instance : Coe Float32 (FmtArg .lowerExp) := LowerExp.coe
instance : Coe Float32 (FmtArg .upperExp) := UpperExp.coe

end Fmtl
