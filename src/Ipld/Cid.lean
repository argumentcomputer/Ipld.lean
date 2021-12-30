import Ipld.Multihash
import Ipld.Multibase
import Ipld.UnsignedVarint

structure Cid where
  version : Nat
  codec: Nat
  hash: Multihash
  deriving BEq, Inhabited

namespace Cid
def toBytes (self : Cid) : ByteArray :=
 (UnsignedVarint.toVarInt self.version) ++ (UnsignedVarint.toVarInt self.codec) ++ Multihash.toBytes self.hash

def toString (self: Cid) : String :=
  Multibase.encode Multibase.Base32 (toBytes self).toList

instance : ToString Cid where
  toString := toString

def fromBytes (bytes : ByteArray) : Option Cid :=
  Option.bind (UnsignedVarint.fromVarInt bytes) $ fun (version, bytes) =>
  Option.bind (UnsignedVarint.fromVarInt bytes) $ fun (codec, bytes) =>
  Option.bind (Multihash.fromBytes bytes) $ fun hash =>
  some { version, codec, hash }

namespace Test

def ex1 : Cid := { version := 0x1, codec := 0x11, hash := Multihash.Test.ex1 }

#eval ex1
#eval toBytes ex1
#eval fromBytes (toBytes ex1)

end Test

end Cid
