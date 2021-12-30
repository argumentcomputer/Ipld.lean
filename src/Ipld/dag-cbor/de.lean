import Ipld.Ipld
import Ipld.Cid
import Ipld.Multihash
import Ipld.Utils
import Ipld.UnsignedVarint
import Std.Data.RBTree

open Std (RBNode)

namespace De

def byteSlice (bytes : ByteArray) (lower : Nat) (upper : Nat) : ByteArray :=
  bytes.data[lower:upper].toArray.data.toByteArray
  
--Potential replacement for byteSlice if I can get it working in Subarray namespace
--def asBA (s : Subarray UInt8) : ByteArray :=
--  s.as.data.toByteArray

def parseUInt16 (bytes : ByteArray) : UInt16 :=
  bytes.fromByteArrayBE.toUInt16

def parseUInt32 (bytes : ByteArray) : UInt32 :=
  bytes.fromByteArrayBE.toUInt32
  
def parseUInt64 (bytes : ByteArray) : UInt64 :=
  bytes.fromByteArrayBE.toUInt64

def deserialize_bool (bytes : ByteArray) (bool : Bool) : Ipld := do
  Ipld.bool bool

def deserialize_u8 (byte : UInt8) : Ipld :=
  Ipld.number byte.toUInt64

def deserialize_u16 (bytes : ByteArray) : Ipld :=
  Ipld.number (parseUInt16 bytes).toUInt64

def deserialize_u32 (bytes : ByteArray) : Ipld :=
  Ipld.number (parseUInt32 bytes).toUInt64

def deserialize_u64 (bytes : ByteArray) : Ipld :=
  Ipld.number (parseUInt64 bytes)

def deserialize_bytes : ByteArray -> Nat -> Ipld
| bytes, 0 => Ipld.byte { data := #[] }
| bytes, len => Ipld.byte (byteSlice bytes 0 len)
  
def deserialize_string : ByteArray -> Nat -> Ipld
| bytes, 0 => Ipld.string ""
| bytes, len => Ipld.string (byteSlice bytes 0 len).toList.toString

def deserialize_link (bytes : ByteArray) : Ipld := do
  let multibase_prefix := bytes[0]
  let bytes := byteSlice bytes 1 bytes.size
  --if multibase_prefix != 0
  --then Err(InvalidCidPrefix) -- TODO
  --then Ipld.link (Cid.fromBytes { data := #[] }).get!
  --else
  Ipld.link (Cid.fromBytes bytes).get!
  
mutual

partial def deserialize (bytes : ByteArray) : Ipld := do
  let code := bytes[0]
  match code with
  | 0x18 => deserialize_u8 bytes[1]
  | 0x19 => deserialize_u16 (byteSlice bytes 1 3)
  | 0x1a => deserialize_u32 (byteSlice bytes 1 5)
  | 0x1b => deserialize_u64 (byteSlice bytes 1 9)
  | 0x58 => deserialize_bytes (byteSlice bytes 2 bytes.size) bytes[1].toNat
  | 0x59 => (deserialize_bytes
            (byteSlice bytes 3 bytes.size) (parseUInt16 (byteSlice bytes 1 3)).toNat)
  | 0x5a => (deserialize_bytes
            (byteSlice bytes 5 bytes.size) (parseUInt32 (byteSlice bytes 1 5)).toNat)
  | 0x5b => (deserialize_bytes
            (byteSlice bytes 9 bytes.size) (parseUInt64 (byteSlice bytes 1 9)).toNat)
  | 0x78 => deserialize_string (byteSlice bytes 2 bytes.size) bytes[1].toNat
  | 0x79 => (deserialize_string
            (byteSlice bytes 3 bytes.size) (parseUInt16 (byteSlice bytes 1 3)).toNat)
  | 0x7a => (deserialize_string
            (byteSlice bytes 5 bytes.size) (parseUInt32 (byteSlice bytes 1 5)).toNat)
  | 0x7b => (deserialize_string
            (byteSlice bytes 9 bytes.size) (parseUInt64 (byteSlice bytes 1 9)).toNat)
  --| 0x98 => deserialize_array (byteSlice bytes 2 bytes.size) bytes[1].toNat
  --| 0x99 => (deserialize_array
  --          (byteSlice bytes 3 bytes.size) (parseUInt16 (byteSlice bytes 1 3)).toNat)
  --| 0x9a => (deserialize_array
  --          (byteSlice bytes 5 bytes.size) (parseUInt32 (byteSlice bytes 1 5)).toNat)
  --| 0x9b => (deserialize_array
  --          (byteSlice bytes 9 bytes.size) (parseUInt64 (byteSlice bytes 1 9)).toNat)
  --| 0xb8 => deserialize_map (byteSlice bytes 2 bytes.size) bytes[1].toNat
  --| 0xb9 => (deserialize_map
  --          (byteSlice bytes 3 bytes.size) (parseUInt16 (byteSlice bytes 1 3)).toNat)
  --| 0xba => (deserialize_map
  --          (byteSlice bytes 5 bytes.size) (parseUInt32 (byteSlice bytes 1 5)).toNat)
  --| 0xbb => (deserialize_map
  --          (byteSlice bytes 9 bytes.size) (parseUInt64 (byteSlice bytes 1 9)).toNat)
  | 0xd8 =>
    if bytes[1] == 0x2a
    then
      match deserialize (byteSlice bytes 2 bytes.size) with
      | Ipld.byte b => deserialize_link b
      | _ => Ipld.link (Cid.fromBytes { data := #[] }).get!
    else Ipld.link (Cid.fromBytes { data := #[] }).get!
  | 0xf4 => deserialize_bool bytes Bool.false
  | 0xf5 => deserialize_bool bytes Bool.true
  | 0xf6 => Ipld.null
  | 0xf7 => Ipld.null
  | x =>
    if x <= 0x17 then
      Ipld.number code.toUInt64
    else if x >= 0x40 && x <= 0x57 then
      deserialize_bytes (byteSlice bytes 1 bytes.size) (code - 0x40).toNat
    else if x >= 0x60 && x <= 0x77 then
      deserialize_string (byteSlice bytes 1 bytes.size) (code - 0x60).toNat
    --else if x >= 0x80 && x <= 0x97 then
    --  deserialize_array (byteSlice bytes 1 bytes.size) (code - 0x80).toNat
    --else if x >= 0xb0 && x <= 0xb7 then
    --  deserialize_map (byteSlice bytes 1 bytes.size) (code - 0xa0).toNat
    else
      Ipld.null

--partial def deserialize_array (bytes : ByteArray) (len : Nat) : Ipld := do
--  let mut array : Array Ipld := { data := [] }
--  for item in [:len] do
--    array.push (deserialize bytes)
--  Ipld.array array

--partial def deserialize_map (bytes : ByteArray) (len : Nat) : Ipld := do
--  let mut map : List (String × Ipld) := []
--  --let mut map : RBNode String (fun _ => Ipld) := RBNode.leaf
--  for i in [:len] do
--    -- Deserialize key, must be a text string
--    if bytes[i] < 0x60 || bytes[i] > 0x7b
--    then return Ipld.mkObject map
--    match deserialize bytes with
--    | Ipld.string key => do
--      -- Deserialize value into IPLD object
--      let val : Ipld := deserialize bytes
--      map.append [(key, val)]
--    | _ => return Ipld.mkObject map
--  Ipld.mkObject map
  
end

namespace Test

#eval deserialize { data := #[0xf6] } == Ipld.null
#eval deserialize { data := #[0xf5] } == Ipld.bool true
#eval deserialize { data := #[0x1] } == Ipld.number 1
#eval deserialize Ser.Test.bytes_ex1_encoded == Ipld.byte "Hello".toUTF8
#eval deserialize Ser.Test.string_ex1_encoded == Ipld.string "Hello"
#eval deserialize Ser.Test.array_ex1_encoded == Ipld.array Ser.Test.array_ex1
#eval deserialize Ser.Test.object_ex1_encoded == Ipld.object Ser.Test.object_ex1
#eval deserialize Ser.Test.cid_ex1_encoded == Ipld.link Ser.Test.cid_ex1

end Test

end De
