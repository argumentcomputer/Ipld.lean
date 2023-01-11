import Ipld.UnsignedVarInt
import Ipld.Multibase
import Ipld.Keccak
import YatimaStdLib.ByteArray

instance : Repr ByteArray where
  reprPrec x prec := Repr.addAppParen ("ByteArray " ++ toString x.data) prec

structure Multihash where
  code : Nat
  size : Nat
  digest : ByteArray
  deriving BEq, Inhabited, Repr

namespace Multihash

def toBytes (self : Multihash) : ByteArray :=
  (UnsignedVarInt.toVarInt self.code) ++ (UnsignedVarInt.toVarInt self.size) ++ self.digest

def toString (self : Multihash) : String :=
  Base.b64.toMultibase.encode (toBytes self).toList

instance : ToString Multihash where
  toString := toString

def fromBytes (bytes : ByteArray) : Option Multihash :=
  (UnsignedVarInt.fromVarInt bytes).bind $ fun (code, bytes) =>
    (UnsignedVarInt.fromVarInt bytes).bind $ fun (size, bytes) =>
      if bytes.size > size then none
      else some ⟨code, size, bytes⟩

def sha3_256 (x: ByteArray) : Multihash :=
  { code := 0x16, size := 32, digest := Keccak.sha3_256 x }

def sha3_512 (x: ByteArray) : Multihash :=
  { code := 0x14, size := 64, digest := Keccak.sha3_512 x }

end Multihash
