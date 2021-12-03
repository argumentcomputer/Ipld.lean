import Ipld.Multihash
import Ipld.Multibase

structure Cid where
  version : Nat
  codec: Nat
  hash: Multihash
  deriving Inhabited

namespace Cid
def toBytes (self : Cid) : List UInt8 :=
 (toVarInt self.version) ++ (toVarInt self.codec) ++ Multihash.toBytes self.hash

def toString (self: Cid) : String :=
  Multibase.encode Multibase.Base32 (toBytes self)

instance : ToString Cid where
  toString := toString

def fromBytes (bytes : List UInt8) : Option Cid :=
  Option.bind (readVarInt bytes) $ fun (version, bytes) =>
  Option.bind (readVarInt bytes) $ fun (codec, bytes) =>
  Option.bind (Multihash.fromBytes bytes) $ fun hash =>
  some { version, codec, hash}

namespace Test

def ex1 : Cid := { version := 0x1, codec := 0x11, hash := Multihash.Test.ex1 }

#eval ex1
#eval toBytes ex1
#eval fromBytes (toBytes ex1)


end Test

end Cid
