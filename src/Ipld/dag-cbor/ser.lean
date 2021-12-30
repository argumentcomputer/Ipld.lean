import Ipld.Ipld
import Ipld.Cid
import Ipld.Utils
import Ipld.De
import Std.Data.RBTree

open Std (RBNode)

namespace Ser

def serialize_null (self : ByteArray) : ByteArray :=
  self.push 0xf6

def serialize_bool (self : ByteArray) (bool : Bool) : ByteArray :=
  match bool with
  | true => self.push 0xf5
  | false => self.push 0xf4

def serialize_u8 (self : ByteArray) (major : UInt8) (n : UInt8) : ByteArray :=
  if n <= 0x17
  then self.push ((major.toNat.shiftLeft 5).lor n.toNat).toUInt8
  else self.append { data := #[((major.toNat.shiftLeft 5).lor 24).toUInt8, n] }
  
def serialize_u16 (self : ByteArray) (major : UInt8) (n : UInt16) : ByteArray :=
  --if n <= UInt8.max
  if n <= 255
  then serialize_u8 self major n.toUInt8
  else
    let buf : ByteArray := { data := #[((major.toNat.shiftLeft 5).lor 25).toUInt8] }
    let bytes : ByteArray := n.toNat.toByteArrayBE
    ByteArray.append self (ByteArray.append buf bytes)

def serialize_u32 (self : ByteArray) (major: UInt8) (n : UInt32) : ByteArray :=
  --if n <= UInt16.max
  if n <= 65535
  then serialize_u16 self major n.toUInt16
  else
    let buf := { data := #[((major.toNat.shiftLeft 5).lor 26).toUInt8] }
    let bytes : ByteArray := n.toNat.toByteArrayBE
    ByteArray.append self (ByteArray.append buf bytes)

def serialize_u64 (self : ByteArray) (major: UInt8) (n : UInt64) : ByteArray :=
  --if n < UInt32.max
  if n <= 4294967295
  then serialize_u32 self major n.toUInt32
  else do
    let buf := { data := #[((major.toNat.shiftLeft 5).lor 27).toUInt8] }
    let bytes : ByteArray := n.toNat.toByteArrayBE
    ByteArray.append self (ByteArray.append buf bytes)

def serialize_string (self : ByteArray) (s: String) : ByteArray := do
  let str_bytes := s.toUTF8
  let self := serialize_u64 self 3 str_bytes.size.toUInt64
  self.append str_bytes

def serialize_bytes (self : ByteArray) (b: ByteArray) : ByteArray := do
  let self := serialize_u64 self 2 b.size.toUInt64
  self.append b

def serialize_link (self : ByteArray) (l: Cid) : ByteArray := do
  let mut self := serialize_u64 self 6 42
  let cid := ByteArray.append { data := #[0] } (Cid.toBytes l)
  serialize_bytes self cid

def nodeToList (map : RBNode String (fun _ => Ipld)) : List (String × Ipld) := 
  map.revFold (fun as a b => (a,b)::as) []

mutual

partial def serialize : ByteArray -> Ipld -> ByteArray
  | self, Ipld.null => serialize_null self
  | self, Ipld.bool b => serialize_bool self b
  | self, Ipld.number n => serialize_u64 self 0 n
  | self, Ipld.string s => serialize_string self s
  | self, Ipld.byte b => serialize_bytes self b
  | self, Ipld.array a => serialize_array self a
  | self, Ipld.object o => serialize_object self o
  | self, Ipld.link cid => serialize_link self cid

partial def serialize_array (self : ByteArray) (a: Array Ipld) : ByteArray := do
  let mut self := serialize_u64 self 4 a.size.toUInt64
  for i in [:a.size] do
    self := serialize self a[i]
  self

partial def serialize_object (self : ByteArray) (o: RBNode String (fun _ => Ipld)) : ByteArray := do
  let list := nodeToList o
  let mut self := serialize_u64 self 5 list.length.toUInt64
  for (k, v) in list do
    self := serialize_string self k
    self := serialize self v
  self

end

namespace Test

def string_ex1 := "Hello"
def string_ex1_encoded : ByteArray := { data := #[0x65, 0x48, 0x65, 0x6c, 0x6c, 0x6f] }
def bytes_ex1_encoded : ByteArray := { data := #[0x45, 0x48, 0x65, 0x6c, 0x6c, 0x6f] }
def array_ex1 : Array Ipld := { data := [Ipld.string "Hello"] }
def array_ex1_encoded : ByteArray := { data := #[0x81, 0x65, 0x48, 0x65, 0x6c, 0x6c, 0x6f]}
def object_ex1 : RBNode String (fun _ => Ipld) := RBNode.singleton "Hello" (Ipld.string "World")
def object_ex1_encoded : ByteArray := { data := #[0xa1, 0x65, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x65, 0x57, 0x6f, 0x72, 0x6c, 0x64] }
def hash_ex1 : Multihash := { code := 0x11, size := 0x4,  digest := { data := #[0b10110110, 0b11111000, 0b01011100, 0b10110101]}}
def cid_ex1 : Cid := { version := 0x1, codec := 0x11, hash := hash_ex1 }
def cid_ex1_encoded : ByteArray := { data := #[0xd8, 0x2a, 0x49, 0x0, 0x1, 0x11, 0x11, 0x4, 0xb6, 0xf8, 0x5c, 0xb5] }

#eval serialize { data := #[] } Ipld.null == { data := #[0xf6] }
#eval serialize { data := #[] } (Ipld.bool true) == { data := #[0xf5] }
#eval serialize { data := #[] } (Ipld.number 1) == { data := #[0x1] }
#eval serialize { data := #[] } (Ipld.string "Hello") == string_ex1_encoded
#eval serialize { data := #[] } (Ipld.byte "Hello".toUTF8) == bytes_ex1_encoded
#eval serialize { data := #[] } (Ipld.array array_ex1) == array_ex1_encoded
#eval serialize { data := #[] } (Ipld.object object_ex1) == object_ex1_encoded
#eval serialize { data := #[] } (Ipld.link cid_ex1) == cid_ex1_encoded

--Roundtrip testing
#eval De.deserialize (serialize Ipld.null) == Ipld.null
#eval De.deserialize (serialize Ipld.bool true) == Ipld.bool true
#eval De.deserialize (serialize Ipld.number 1) == Ipld.number 1
#eval De.deserialize (serialize Ipld.string "Hello") == Ipld.string "Hello"
#eval De.deserialize (serialize Ipld.byte "Hello".toUTF8) == Ipld.byte "Hello".toUTF8
#eval De.deserialize (serialize Ipld.array array_ex1) == Ipld.array array_ex1
#eval De.deserialize (serialize Ipld.object object_ex1) == Ipld.object object_ex1
#eval De.deserialize (serialize Ipld.cid cid_ex1) == Ipld.cid cid_ex1

end Test

end Ser
