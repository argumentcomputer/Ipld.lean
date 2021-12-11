-- TODO:
-- parse_f64 and deserialize_indefinites
-- Make generic parse_UInt and deserialize_number with Nat
-- Make a byte array structure with parse/deserialize functions as impls
-- Only pass bytes up to given length instead of the whole array
-- ByteArray length for CID?
-- Add error handling, especially when reading up to given length
-- Add testing

import Ipld.Ipld
import Ipld.Cid
import Ipld.Multihash
import Ipld.Utils
import Std.Data.RBTree

open Std (RBNode)

def deserialize (bytes: ByteArray) : Ipld
  match bytes[0] with
  -- Unsigned integer types
    | 0x00..=0x17 => deserialize_u8 bytes[0]
  | 0x18 => deserialize_u8 bytes[1]
  | 0x19 => deserialize_u16 (parse_u16 bytes[1:3])
  | 0x1a => deserialize_u32 (parse_u32 bytes[1:5])
  | 0x1b => deserialize_u64 (parse_u64 bytes[1:9])
  | 0x1c..0x1f => Err(UnassignedCode)
  -- Negative integer types
  -- Unused for now
  -- | 0x20..=0x37 => deserialize_i8(-1 - (bytes[1] - 0x20) as i8)
  -- | 0x38 => deserialize_i16(-1 - bytes[1].toInt16)
  -- | 0x39 => deserialize_i32((-1 - parse_u16(bytes[1:3]).toInt32)
  -- | 0x3a => deserialize_i64((-1 - parse_u32(bytes[1:5]).toInt64)
  -- | 0x3b =>
  --   let value = parse_u64(bytes[1:9])
  --     if value > Int64.max.toUInt64
  --       then deserialize_i128(-1 - value.toInt128)
  --         else deserialize_i64(-1 - value.toInt64)
  -- | 0x3c..=0x3f => Err(UnassignedCode)
  | 0x20..=0x3b => Err(InvalidDagCbor)
  | 0x3c..=0x3f => Err(UnassignedCode)
  -- Byte string types
  | 0x40..=0x57 => deserialize_bytes bytes[1:] (bytes[0].toUInt64 - 0x40)
  | 0x58 => deserialize_bytes bytes[2:] bytes[1].toUInt64
  | 0x59 => deserialize_bytes bytes[4:] (parse_u16 bytes[1:3]).toUInt64
  | 0x5a => deserialize_bytes bytes[6:] (parse_u32 bytes[1:5]).toUInt64
  | 0x5b => deserialize_bytes bytes[10] (parse_u64 bytes[1:9])
  | 0x5c..=0x5e => Err(UnassignedCode)
  | 0x5f => deserialize_indefinite_bytes bytes
  -- Text string types
  | 0x60..=0x77 => deserialize_string bytes[1:] (bytes[0].toUInt64 - 0x60)
  | 0x78 => deserialize_string bytes[2:] bytes[1].toUInt64
  | 0x79 => deserialize_string bytes[4:] (parse_u16 bytes[1:3]).toUInt64
  | 0x7a => deserialize_string bytes[6:] (parse_u32 bytes[1:5]).toUInt64
  | 0x7b => deserialize_string bytes[10:] (parse_u64 bytes[1:9])
  | 0x7c..=0x7e => Err(UnassignedCode)
  | 0x7f => deserialize_indefinite_string bytes
  -- Array types
  | 0x80..=0x97 => deserialize_array bytes[1:] (bytes[0].toUInt64 - 0x80)
  | 0x98 => deserialize_array bytes[2:] bytes[1].toUInt64
  | 0x99 => deserialize_array bytes[4:] (parse_u16 bytes[1:3]).toUInt64
  | 0x9a => deserialize_array bytes[6:] (parse_u32 bytes[1:5]).toUInt64
  | 0x9b => deserialize_array bytes[10:] (parse_u64 bytes[1:9])
  | 0x9c..=0x9e => Err(UnassignedCode)
  | 0x9f => deserialize_indefinite_array bytes
  -- Map types
  | 0xa0..=0xb7 => deserialize_map bytes[1:] (bytes[0].toUInt64 - 0xa0)
  | 0xb8 => deserialize_map bytes[2:] bytes[1].toUInt64
  | 0xb9 => deserialize_map bytes[4:] (parse_u16 bytes[1:3]).toUInt64
  | 0xba => deserialize_map bytes[6:] (parse_u32 bytes[1:5]).toUInt64
  | 0xbb => deserialize_map bytes[10:] (parse_u64 bytes[1:9])
  | 0xbc..=0xbe => Err(UnassignedCode)
  | 0xbf => deserialize_indefinite_map bytes
  -- CBOR tag (must be 42 or 0x2a)
  | 0xc0..=0xd7 => Err(InvalidDagCbor)
  | 0xd8 =>
    if bytes[1] == 0x2a
    then deserialize_link bytes[2:]
    else Err(InvalidTag)
  -- Assume any tags larger than 1 byte are invalid
  | 0xd9..=0xdb => Err(InvalidDagCbor)
  | 0xdc..=0xdf => Err(UnassignedCode)
  -- Floats (64-bit, double precision), True, False, and Null types
  | 0xe0..=0xf3 => Err(UnassignedCode)
  | 0xf4 => deserialize_bool Bool.false
  | 0xf5 => deserialize_bool Bool.true
  | 0xf6..=0xf7 => deserialize_null()
  | 0xf8 => Err(UnassignedCode)
  -- Floats must reject Nan, Infinity, and -Infinity
  | 0xf9..=0xfa => Err(InvalidDagCbor)
  | 0xfb => deserialize_float bytes[1:9]
  | 0xfc..=0xfe => Err(UnassignedCode)
  | 0xff => Err(UnexpectedCode)

def parse_u16 (bytes : ByteArray) : UInt16 :=
  (Utils.fromByteArrayBE bytes).toUInt16
  
def parse_u32 (bytes : ByteArray) : UInt32 :=
  (Utils.fromByteArrayBE bytes).toUInt32

def parse_u64 (bytes : ByteArray) : UInt64 :=
  (Utils.fromByteArrayBE bytes).toUInt64
  
def parse_f64 (bytes : ByteArray) : f64 :=
  --unimplemented

def deserialize_null () : Ipld :=
  Ipld.null
  
def deserialize_bool (bool : Bool) : Ipld :=
  Ipld.bool := bool

def deserialize_u8 (num : UInt8) : Ipld :=
  Ipld.number := num.toUInt64

def deserialize_u16 (num : UInt16) : Ipld :=
  Ipld.number := num.toUInt64

def deserialize_u32 (bytes : UInt32) : Ipld :=
  Ipld.number := num.toUInt64

def deserialize_u64 (bytes : UInt64) : Ipld :=
  Ipld.number := num
  
def deserialize_bytes (bytes : ByteArray, len : UInt64) : Ipld :=
  Ipld.byte := bytes[:len]
  
def deserialize_string (bytes : ByteArray, len : UInt64) : Ipld :=
  Ipld.string := bytes[:len].ToString
  
def deserialize_array ( bytes : ByteArray, len : UInt64) : Ipld :=
  Ipld.array := bytes[:len].toArray

-- TODO
def deserialize_map ( bytes : ByteArray, len : UInt64) : Ipld := do
  let map := kvPairs {}
  for elem in bytes[]
    let (key, val) := elem
    map.insert elem
  Ipld.object := map

-- TODO
def deserialize_link ( bytes : ByteArray) : Ipld := do
  let ver := 0x01
  let code := 0x01
  let mh := Multihash.fromBytes bytes[0:1]
  let cid := Cid { version := ver, codec := code, hash := mh }
  Ipld.link := cid
