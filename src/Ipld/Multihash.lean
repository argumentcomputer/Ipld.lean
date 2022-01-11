import Ipld.Utils
import Ipld.UnsignedVarint
import Ipld.Multibase
import Ipld.Keccak

instance : Repr ByteArray where
  reprPrec x prec := Repr.addAppParen ("ByteArray " ++ toString x.data) prec

structure Multihash where
  code : Nat
  size : Nat
  digest : ByteArray
  deriving BEq, Inhabited, Repr

namespace Multihash

def toBytes (self : Multihash) : ByteArray :=
  (UnsignedVarint.toVarInt self.code) ++ (UnsignedVarint.toVarInt self.size) ++ self.digest

def toString (self: Multihash) : String :=
  Multibase.encode Multibase.Base64 (toBytes self).toList

instance : ToString Multihash where
  toString := toString

def fromBytes (bytes : ByteArray) : Option Multihash := do
  Option.bind (UnsignedVarint.fromVarInt bytes) $ fun (code, bytes) =>
  Option.bind (UnsignedVarint.fromVarInt bytes) $ fun (size, bytes) =>
  let digest := bytes
  if digest.size > size then none
  else some { code, size, digest }

def sha3_256 (x: ByteArray) : Multihash :=
  {code := 0x16, size := 32, digest := Keccak.sha3_256 x }

namespace Test

def ex1 : Multihash := { code := 0x11, size := 0x4, digest := { data := #[0b10110110, 0b11111000, 0b01011100, 0b10110101] }}

#eval ex1
#eval toBytes ex1
#eval fromBytes (toBytes ex1)

end Test
end Multihash
