import Ipld.MultibaseImpl

import YatimaStdLib.Nat

/-- An instance of the Multibase specification -/
structure Multibase where
  code    : Char
  alpha   : String
  digit   : Nat → Char
  read    : Char → Option Nat
  rfc4648 : Bool
  pad     : Bool

namespace Multibase

variable (m : Multibase)

def zero : Char :=
  (m.alpha.data)[0]!

def base : Nat  :=
  m.alpha.length

def log2Base : Nat :=
  m.base.log2'

-- Every RFC4648 base has a group size which is least-common-multiple of the
-- number of bits per digit of the base (`log2Base`) and 8 (the number of bits
-- per byte). This function returns the size of the group in bits.
def group : Nat :=
  let x := m.log2Base
  if x % 8 == 0 then x else
  if x % 4 == 0 then x * 2 else
  if x % 2 == 0 then x * 4 else
  x * 8

-- This is a little slow. We gain some in-kernel performance by
-- requiring Multibase instances to hardcode a `digit` function, even though
-- semantically it's derivable from `alpha`
def digit' (i : Nat): Char :=
  if h : i < m.alpha.length
    then m.alpha.data[i]
    else m.zero

-- This is very slow because of the String.posOf call. We can't reduce
-- encodings in-kernel unless we hardcode the `read` function in the instance
def read' (c : Char): Option Nat :=
  let x := String.posOf m.alpha c
  if x == m.alpha.endPos then none else some x.byteIdx

def validDigit (x : Char): Bool :=
  m.read x != none

def validate (x : String): Bool :=
  x.data.all m.validDigit

-- The core encoding function operates over `Nat` (GMP numbers) which is
-- supported in the Lean Kernel, thus allowing more complex static checking
-- compared to ByteArray operations (which are currently unsupported).,
def toStringCore (str : String) : Nat → Nat → String
  | 0,        _ => str
  | _ + 1,    0 => str
  | fuel + 1, n =>
    let dig := m.digit (n % m.base)
    toStringCore ((String.singleton dig) ++ str) fuel (n / m.base)

def toString (x : List UInt8): String :=
  match Nat.fromByteListBE x with
  | 0 => ""
  | n => m.toStringCore "" (n + 1) n

def padRight (input : String): Nat → String
  | 0     => input
  | n + 1 => padRight (String.push input '=') n

def leadingZeroBitsCore : Nat → List UInt8 → Nat → Nat
  | 0,        _,       n => n
  | _ + 1,    [],      n => n
  | fuel + 1, 0 :: bs, n => leadingZeroBitsCore fuel bs (n + 8)
  | _ + 1,    b :: _,  n => n + (8 - Nat.sigBits b.toNat)

def leadingZeroBits (bytes : List UInt8) : Nat :=
  leadingZeroBitsCore (bytes.length) bytes 0

def encode (x : List UInt8): String :=
  let log := m.log2Base                        -- number of bits per digit
  let lzs := leadingZeroBits x                 -- count leading zero bits
  let rfc := m.rfc4648                         -- RFC4648 conformance
  let zeros := String.mk $ if rfc              -- if in RFC4648
    then List.replicate (lzs / log) m.zero     -- left zeros are normal digits
    else List.replicate (lzs / 8) m.zero       -- else, they count whole bytes
  let grp := m.group / 8                       -- RFC group size in bytes
  let bytePad := (grp - x.length % grp) % grp  -- for counting right pad bytes
  let x := if rfc                              -- in RFC4648,
    then x ++ List.replicate bytePad 0         -- push right zero byte pad
    else x                                     -- else, do nothing
  let n := Nat.fromByteListBE x                -- convert bytes to big number
  let str := m.toStringCore "" (n + 1) n       -- core conversion loop
  let charPad := (bytePad * 8) / log           -- the pad size in characters
  let str' := if rfc                           -- in RFC4648
    then str.dropRight charPad                 -- drop the character pad size
    else str                                   -- else, do nothing
  let str' := if rfc && m.pad                  -- in RFC4648 with explicit pad
    then padRight str' charPad                 -- add back the pad characters
    else str'                                  -- else, do nothing
  String.singleton m.code ++ zeros ++ str'     -- return w/ base code & zeros

def fromPad : Nat → Nat → Nat → String → Option (Nat × Nat)
  | 0,        pad, acc, _     => Option.some (pad, acc)
  | _ + 1,    pad, acc, ""    => Option.some (pad, acc)
  | fuel + 1, pad, acc, input =>
    if (input.data[0]! == '=')
    then fromPad fuel (pad + 1) (acc * m.base) (String.drop input 1)
    else Option.none

def fromStringCore : Nat → Nat → String → Option (Nat × Nat)
  | 0,        acc, _ => Option.some (0, acc)
  | _ + 1,    acc, "" => Option.some (0, acc)
  | fuel + 1, acc, input =>
    if input.data[0]! == '='
    then (m.fromPad (fuel + 1) 0 acc input)
    else Option.bind (m.read input.data[0]!) (fun d =>
      fromStringCore fuel (acc * m.base + d) (input.drop 1))

def fromString : String → Option (List UInt8)
  | "" => some []
  | s  => (fun (_, y) => Nat.toByteListBE y) <$> (m.fromStringCore s.length 0 s)

def readCode : String → Option String
  | ⟨c::cs⟩ => if c == m.code
    then some (String.mk cs)
    else none
  | _ => none

def readZeros : List Char → Nat
  | c::cs => if c == m.zero
    then 1 + readZeros cs
    else 0
  | _ => 0

def decode (input : String): Option (List UInt8) :=
  Option.bind (m.readCode input) $ fun x =>
  let len := x.length
  let zeroChars := m.readZeros x.data
  let log := m.log2Base
  let x := if m.rfc4648
    then
      -- Every rfc4648 base has a "group", e.g. 5 bytes or 40 bits for Base32
      -- which is is the least common multiple of 8 and the bits per base char
      -- This is so the characters and bytes line up evenly, and padding is
      -- introduced when there are insufficient input bits to fill a group.
      let grp     := m.group                   -- bits per group
      let chars   := grp / log                 -- chars per group
      let inBits  := len * log                 -- bits in input
      let remBits := inBits % grp              -- excess input bits
      let pad     := (grp - remBits) / log     -- chars to fill a group
      let pad     := pad % chars               -- but don't fill an extra one
      padRight x pad
    else x
  if x = ""
  then Option.some []
  else Option.bind (m.fromStringCore x.length 0 x) $ fun (padLen, x') =>
  let out    := Nat.toByteListBE x'
  let len    := if m.pad then len else len + padLen
  let zeros := if m.rfc4648
    -- In RFC4648, leading zero chars correspond to log2Base β leading bits.
    -- Since the leading significant byte can contain leading zeros bits
    -- we have to check if the bits of the zeroChars and the decoded bytes
    -- don't have enough bits to cover the size of the input
    then if len * log > out.length * 8 + zeroChars * log
      then (zeroChars * log) / 8 + 1
      else (zeroChars * log) / 8
    -- outside of RFC4648, then a leading zero char *is* a leading zero byte
    else zeroChars
  let padBytes := if (padLen * log) % 8 == 0
    then padLen * log / 8
    else ((padLen * log) / 8) + 1
  let zeros : List UInt8 := List.replicate zeros 0
  some (zeros ++ (out.take (out.length - padBytes)))

--/-- Top level multibase encoding function -/
def encodeBytes (input : ByteArray) : String :=
  m.encode input.data.data

--/-- Top level multibase decoding function -/
def decodeBytes (x : String) : Option ByteArray :=
  ByteArray.mk <$> Array.mk <$> m.decode x

end Multibase

inductive Base
  | b2
  | b8
  | b10

  | b16 | b16Upper

  | b32 | b32Hex   | b32HexUpper | b32HexPad | b32HexPadUpper
  | b32Upper | b32Pad | b32PadUpper | b32Z

  | b36 | b36Upper

  | b58BTC | b58Flickr

  | b64 | b64Pad | b64URL | b64URLPad

def Base.toMultibase : Base → Multibase
  | b2 => {
    code := '0'
    alpha := "01"
    digit := digitBase2
    read := readBase2
    -- This seems not completely right given the multiformats base2 rfc
    rfc4648 := true
    pad := false }
  | b8 => {
    code := '7'
    alpha := "01234567"
    digit := digitBase8
    read := readBase8
    rfc4648 := true
    pad := false }
  | b10 => {
    code := '9'
    alpha := "0123456789"
    digit := digitBase10
    read := readBase10
    rfc4648 := false
    pad := false }
  | b16 => {
    code := 'f'
    alpha := "0123456789abcdef"
    digit := digitBase16
    read := readBase16
    rfc4648 := true
    pad := false }
  | b16Upper => {
    code := 'F'
    alpha := "0123456789ABCDEF"
    digit := digitBase16Upper
    read := readBase16
    rfc4648 := true
    pad := false }
  | b32Hex => {
    code := 'v'
    alpha := "0123456789abcdefghijklmnopqrstuv"
    digit := digitBase32Hex
    read := readBase32Hex
    rfc4648 := true
    pad := false }
  | b32HexUpper => {
    code := 'V'
    alpha := "0123456789ABCDEFGHIJKLMNOPQRSTUV"
    digit := digitBase32HexUpper
    read := readBase32Hex
    rfc4648 := true
    pad := false }
  | b32HexPad => {
    code := 't'
    alpha := "0123456789abcdefghijklmnopqrstuv"
    digit := digitBase32Hex
    read := readBase32Hex
    rfc4648 := true
    pad := true }
  | b32HexPadUpper => {
    code := 'T'
    alpha := "0123456789ABCDEFGHIJKLMNOPQRSTUV"
    digit := digitBase32HexUpper
    read := readBase32Hex
    rfc4648 := true
    pad := true }
  | b32 => {
    code := 'b'
    alpha := "abcdefghijklmnopqrstuvwxyz234567"
    digit := digitBase32
    read := readBase32
    rfc4648 := true
    pad := false }
  | b32Upper => {
    code := 'B'
    alpha := "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
    digit := digitBase32Upper
    read := readBase32
    rfc4648 := true
    pad := false }
  | b32Pad => {
    code := 'c'
    alpha := "abcdefghijklmnopqrstuvwxyz234567"
    digit := digitBase32
    read := readBase32
    rfc4648 := true
    pad := true }
  | b32PadUpper => {
    code := 'C'
    alpha := "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
    digit := digitBase32Upper
    read := readBase32
    rfc4648 := true
    pad := true }
  | b32Z => {
    code := 'h'
    alpha := "ybndrfg8ejkmcpqxot1uwisza345h769"
    digit := digitBase32Z
    read := readBase32Z
    rfc4648 := true
    pad := false }
  | b36 => {
    code := 'k'
    alpha := "0123456789abcdefghijklmnopqrstuvwxyz"
    digit := digitBase36
    read := readBase36
    rfc4648 := false
    pad := false }
  | b36Upper => {
    code := 'K'
    alpha := "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    digit := digitBase36Upper
    read := readBase36
    rfc4648 := false
    pad := false }
  | b58Flickr => {
    code := 'Z'
    alpha := "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ"
    digit := digitBase58Flickr
    read := readBase58Flickr
    rfc4648 := false
    pad := false }
  | b58BTC => {
    code := 'z'
    alpha := "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    digit := digitBase58BTC
    read := readBase58BTC
    rfc4648 := false
    pad := false }
  | b64 => {
    code := 'm'
    alpha := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    digit := digitBase64
    read := readBase64
    rfc4648 := true
    pad := false }
  | b64Pad => {
    code := 'M'
    alpha := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    digit := digitBase64
    read := readBase64
    rfc4648 := true
    pad := true }
  | b64URL => {
    code := 'u'
    alpha := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
    digit := digitBase64URL
    read := readBase64URL
    rfc4648 := true
    pad := false }
  | b64URLPad => {
    code := 'U'
    alpha := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
    digit := digitBase64URL
    read := readBase64URL
    rfc4648 := true
    pad := true }
