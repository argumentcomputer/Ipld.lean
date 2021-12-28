namespace Nat

  def toByteArrayCore : Nat → Nat → ByteArray → ByteArray
  | 0, n, bytes => bytes
  | fuel+1, n, bytes =>
      let b: UInt8 := UInt8.ofNat (n % 256);
      let n' := n / 256;
      if n' = 0 then (bytes.push b)
      else toByteArrayCore fuel n' (bytes.push b)

  /-- Convert Nat to Little-Endian ByteArray -/
  def toByteArrayLE (n: Nat) : ByteArray := 
    toByteArrayCore (n+1) n { data := #[] }

  /-- Convert Nat to Big-Endian ByteArray -/
  def toByteArrayBE (n: Nat) : ByteArray := do
    let bytes := (toByteArrayCore (n+1) n { data := #[]})
    let mut bytes' : ByteArray := { data := #[]}
    for i in [0:bytes.size] do
      bytes' := bytes'.push (bytes.data[bytes.size - 1 - i])
    bytes'

  def toByteListCore : Nat → Nat → List UInt8 → List UInt8
  | 0, n, bytes => bytes
  | fuel+1, n, bytes =>
      let b: UInt8 := UInt8.ofNat (n % 256);
      let n' := n / 256;
      if n' = 0 then (bytes.cons b)
      else toByteListCore fuel n' (bytes.cons b)

  /-- Convert Nat to Big-Endian byte list -/
  def toByteListBE (n: Nat) : List UInt8 := do
    toByteListCore (n+1) n []

  def byteLength (n : Nat) : Nat := n.toByteArrayLE.size


  def fromByteListCore: Nat → List UInt8 → Nat → Nat
  | 0, bytes, n => n
  | fuel+1, [], n => n
  | fuel+1, b::bs, n => 
    fromByteListCore fuel bs (Nat.shiftLeft n 8 + (UInt8.toNat b))

  /-- Read Nat from Big-Endian byte list -/
  def fromByteListBE (b : List UInt8) : Nat :=
    fromByteListCore (b.length + 1) b 0

  def sigBitsCore: Nat → Nat → Nat → Nat
  | 0, acc, n  => acc
  | f+1, acc, 0    => acc
  | f+1, acc, n => sigBitsCore f (acc+1) (n.shiftRight 1)

  /-- Significant Bits in a Nat -/
  def sigBits (x: Nat) : Nat := sigBitsCore x 0 x

  /-- Faster in-kernel log2 -/
  def log2' (x: Nat) : Nat := sigBits x - 1

end Nat

namespace ByteArray

def fromByteArrayLE (b: ByteArray) : Nat := do
  let mut x := 0
  for i in [:b.size] do
    x := x + Nat.shiftLeft (UInt8.toNat b.data[i]) (i * 8)
  return x

/-- Read Nat from Big-Endian ByteArray -/
def fromByteArrayBE (b: ByteArray) : Nat := do
  let mut x := 0
  for i in [:b.size] do
    x := Nat.shiftLeft x 8 + (UInt8.toNat b.data[i])
  return x

def leadingZeroBits (bytes: ByteArray) : Nat := do
  let mut c := 0
  for byte in bytes do
    let zs := (8 - Nat.sigBits (UInt8.toNat byte))
    if byte != 0
    then return c + zs
    else c := c + zs
  return c

def pushZeros (bytes: ByteArray): Nat → ByteArray
| 0 => bytes
| n+1 => pushZeros (bytes.push 0) n

end ByteArray
