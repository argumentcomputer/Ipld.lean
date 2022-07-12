import Ipld.Multihash

structure Cid where
  version : Nat
  codec: Nat
  hash: Multihash
  deriving BEq, Inhabited, Repr

namespace Cid

def toBytes (self : Cid) : ByteArray :=
  (UnsignedVarInt.toVarInt self.version)
    ++ (UnsignedVarInt.toVarInt self.codec)
    ++ (Multihash.toBytes self.hash)

def toString (self: Cid) : String :=
  Base.b32.toMultibase.encode (toBytes self).toList

instance : ToString Cid where
  toString := toString

def fromBytes (bytes : ByteArray) : Option Cid :=
  (UnsignedVarInt.fromVarInt bytes).bind $ fun (version, bytes) =>
    (UnsignedVarInt.fromVarInt bytes).bind $ fun (codec, bytes) =>
      (Multihash.fromBytes bytes).bind $ fun hash =>
        some { version, codec, hash }

instance : Ord Cid where
  compare x y := compare (toBytes x) (toBytes y)

end Cid
