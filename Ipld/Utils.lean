namespace Nat

  def toByteArrayCore : Nat → Nat → ByteArray → ByteArray
  | 0, n, bytes => bytes
  | fuel+1, n, bytes =>
      let b: UInt8 := UInt8.ofNat (n % 256);
      let n' := n / 256;
      if n' = 0 then (bytes.push b)
      else toByteArrayCore fuel n' (bytes.push b)

  def toByteArrayLE (n: Nat) : ByteArray := 
    toByteArrayCore (n+1) n { data := #[] }

  def toByteArrayBE (n: Nat) : ByteArray := do
    let bytes := (toByteArrayCore (n+1) n { data := #[]})
    let mut bytes' : ByteArray := { data := #[]}
    for i in [0:bytes.size] do
      bytes' := bytes'.push (bytes.data[bytes.size - 1 - i])
    bytes'

  def lenBytes (n : Nat) : Nat := n.toByteArrayLE.size

  def fromByteArrayLE (b: ByteArray) : Nat := do
    let mut x := 0
    for i in [:b.size] do
      x := x + Nat.shiftLeft (UInt8.toNat b.data[i]) (i * 8)
    return x

  def fromByteArrayBE (b: ByteArray) : Nat := do
    let mut x := 0
    for i in [:b.size] do
      x := Nat.shiftLeft x 8 + (UInt8.toNat b.data[i])
    return x

  def sigBitsCore: Nat → Nat → Nat → Nat
  | 0, acc, n  => acc
  | f+1, acc, 0    => acc
  | f+1, acc, n => sigBitsCore f (acc+1) (n.shiftRight 1)

  def sigBits (x: Nat) : Nat := sigBitsCore x 0 x

  def log2' (x: Nat) : Nat := sigBits x - 1

end Nat

--#eval Nat.fromByteArrayLE (Nat.toByteArrayLE 12345678910) == 12345678910
--#eval Nat.fromByteArrayBE (Nat.toByteArrayBE 12345678910) == 12345678910

namespace ByteArray

def leadingZeroBits (bytes: ByteArray) : Nat := do
  let mut c := 0
  for byte in bytes do
    let zs := (8 - Nat.sigBits (UInt8.toNat byte))
    if byte != 0 then return c + zs
    else c := c + zs
  return c

def pushZeros (bytes: ByteArray): Nat → ByteArray
| 0 => bytes
| n+1 => pushZeros (bytes.push 0) n

end ByteArray




