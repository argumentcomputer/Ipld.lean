import Ipld.UnsignedVarint
import Ipld.Multibase

structure Multihash where
  code : Nat
  size : Nat
  digest : ByteArray
  deriving BEq, Inhabited

namespace Multihash

def toBytes (self : Multihash) : ByteArray :=
  (toVarInt self.code) ++ (toVarInt self.size) ++ self.digest

def toString (self: Multihash) : String :=
  Multibase.encode Multibase.Base64 (toBytes self)

instance : ToString Multihash where
  toString := toString

inductive Error
| BadDigestSize (x: Nat) (y: Nat)
deriving BEq

instance [BEq ε] [BEq α] : BEq (Except ε α) where
  beq a b := match a, b with
  | Except.ok a, Except.ok b => a == b
  | Except.error a, Except.error b => a == b
  | _, _ => false

def fromBytes (bytes : ByteArray) : Except Error Multihash := do
  let code := readVarInt bytes $ fun (code, bytes)
  let size := readVarInt bytes $ fun (size, bytes)
  let digest := bytes
  if digest.length > size
  then Except.error (Error.BadDigestSize digest.length size) 
  else return { code := code, size := size, digest := digest }
  
namespace Test

def ex1 : Multihash := { code := 0x11, size := 0x4,  digest := [0b10110110, 0b11111000, 0b01011100, 0b10110101]}

#eval ex1
#eval toBytes ex1
#eval fromBytes (toBytes ex1)

end Test
end Multihash
