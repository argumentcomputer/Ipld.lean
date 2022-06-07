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
  Multibase.encode Multibase.Base32 (toBytes self).toList

instance : ToString Cid where
  toString := toString

def fromBytes (bytes : ByteArray) : Option Cid := do
  let (version, bytes) <- UnsignedVarInt.fromVarInt bytes;
  let (codec, bytes) <- UnsignedVarInt.fromVarInt bytes;
  let hash <- Multihash.fromBytes bytes;
  some { version, codec, hash }

def fromString [Multibase β] (string: String) : Option Cid := do
  let bytes <- Multibase.decodeBytes β string;
  fromBytes bytes

instance : Ord Cid where
  compare x y := compare (toBytes x) (toBytes y)

end Cid
