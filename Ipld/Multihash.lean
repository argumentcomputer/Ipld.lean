import Ipld.UnsignedVarint
import Ipld.Multibase

structure Multihash where
  code : Nat
  size : Nat
  digest : List UInt8
  deriving BEq, Inhabited



namespace Multihash

def toBytes (self : Multihash) : List UInt8 :=
  (toVarInt self.code) ++ (toVarInt self.size) ++ self.digest

def toString (self: Multihash) : String :=
  Multibase.encode Multibase.Base64 (toBytes self)

instance : ToString Multihash where
  toString := toString

def fromBytes (bytes : List UInt8) : Option Multihash :=
  Option.bind (readVarInt bytes) $ fun (code, bytes) =>
  Option.bind (readVarInt bytes) $ fun (size, bytes) =>
  let digest := bytes
  if digest.length > size then none
  else some { code := code, size := size, digest := digest }

namespace Test

def ex1 : Multihash := { code := 0x11, size := 0x4,  digest := [0b10110110, 0b11111000, 0b01011100, 0b10110101]}

#eval ex1
#eval toBytes ex1
#eval fromBytes (toBytes ex1)

end Test
end Multihash
