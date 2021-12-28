-- TODO:
-- Add an internal cursor/iterator instead of incrementing Deserializer index
-- Add error handling, especially when reading up to given length
-- Add testing
-- Change UInt64 to Nat where necessary

import Ipld.Ipld
import Ipld.Cid
import Ipld.Multihash
import Ipld.Utils
import Ipld.UnsignedVarint
import Std.Data.RBTree

open Std (RBNode)

structure Deserializer where
  bytes : ByteArray
  i : UInt64

def deserialize (self : Deserializer) : Ipld
  match self.bytes[self.i] with
  -- Unsigned integer
  | 0x00..=0x17 => Ipld.number self.bytes[self.i].toUInt64
  | 0x18 => deserialize_u8 self
  | 0x19 => deserialize_u16 self
  | 0x1a => deserialize_u32 self
  | 0x1b => deserialize_u64 self
  | 0x1c..0x1f => Err(UnassignedCode)
  -- Negative integer, unused for now
  -- | 0x20..=0x37 => deserialize_i8 (-1 - (self.bytes[self.i+1] - 0x20).toInt8)
  -- | 0x38 => deserialize_i16 (-1 - self.bytes[self.i+1].toInt16)
  -- | 0x39 => deserialize_i32 (-1 - (parse_u16 self.bytes[self.i+1:self.i+3]).toInt32)
  -- | 0x3a => deserialize_i64 (-1 - (parse_u32 self.bytes[self.i+1:self.i+5]).toInt64)
  -- | 0x3b =>
  --   let value = parse_u64 self.bytes[self.i+1:self.i+9]
  --     if value > Int64.max.toUInt64
  --     then deserialize_i128 (-1 - value.toInt128)
  --     else deserialize_i64 (-1 - value.toInt64)
  -- | 0x3c..=0x3f => Err(UnassignedCode)
  -- Byte string/array
  -- args: Deserializer, index incrementor, length
  | 0x40..=0x57 => deserialize_bytes self 1 (self.bytes[self.i] - 0x40).toUInt64
  | 0x58 => deserialize_bytes self 2 (self.bytes[self.i+1]).toUInt64
  | 0x59 => deserialize_bytes self 3 (parse_u16 self.bytes[self.i+1:self.i+3]).toUInt64
  | 0x5a => deserialize_bytes self 5 (parse_u32 self.bytes[self.i+1:self.i+5]).toUInt64
  | 0x5b => deserialize_bytes self 9 (parse_u64 self.bytes[self.i+1:self.i+9])
  | 0x5c..=0x5e => Err(UnassignedCode)
  --| 0x5f => deserialize_indefinite_bytes bytes
  -- Text string
  | 0x60..=0x77 => deserialize_string self 1 (self.bytes[self.i] - 0x60).toUInt64
  | 0x78 => deserialize_string self 2 (self.bytes[self.i]).toUInt64
  | 0x79 => deserialize_string self 3 (parse_u16 self.bytes[self.i+1:self.i+3]).toUInt64
  | 0x7a => deserialize_string self 5 (parse_u32 self.bytes[self.i+1:self.i+5]).toUInt64
  | 0x7b => deserialize_string self 9(parse_u64 self.bytes[self.i+1:self.i+9])
  | 0x7c..=0x7e => Err(UnassignedCode)
  --| 0x7f => deserialize_indefinite_string self.bytes
  -- Array
  | 0x80..=0x97 => deserialize_array self 1 (self.bytes[self.i] - 0x80).toUInt64
  | 0x98 => deserialize_array self 2 self.bytes[self.i+1].toUInt64
  | 0x99 => deserialize_array self 3 (parse_u16 self.bytes[self.i+1:self.i+3]).toUInt64
  | 0x9a => deserialize_array self 5 (parse_u32 self.bytes[self.i+1:self.i+5]).toUInt64
  | 0x9b => deserialize_array self 9 (parse_u64 self.bytes[self.i+1:self.i+9])
  | 0x9c..=0x9e => Err(UnassignedCode)
  --| 0x9f => deserialize_indefinite_array self.bytes
  -- StringMap
  | 0xa0..=0xb7 => deserialize_map 1 (self.bytes[self.i] - 0xa0).toUInt64
  | 0xb8 => deserialize_map 2 self.bytes[self.i+1].toUInt64
  | 0xb9 => deserialize_map 3 (parse_u16 self.bytes[self.i+1:self.i+3]).toUInt64
  | 0xba => deserialize_map 5 (parse_u32 self.bytes[self.i+1:self.i+5]).toUInt64
  | 0xbb => deserialize_map 9 (parse_u64 self.bytes[self.i+1:self.i+9])
  | 0xbc..=0xbe => Err(UnassignedCode)
  --| 0xbf => deserialize_indefinite_map self.bytes
  -- CBOR tag (must be 42 or 0x2a)
  | 0xc0..=0xd7 => Err(InvalidDagCbor)
  | 0xd8 =>
    if self.bytes[self.i+1] == 0x2a
    then deserialize_link self
    else Err(InvalidTag)
  -- Assume any tags larger than 1 byte are invalid
  | 0xd9..=0xdb => Err(InvalidDagCbor)
  | 0xdc..=0xdf => Err(UnassignedCode)
  | 0xe0..=0xf3 => Err(UnassignedCode)
  -- Bool and Null types
  | 0xf4 => deserialize_bool Bool.false
  | 0xf5 => deserialize_bool Bool.true
  | 0xf6..=0xf7 => deserialize_null()
  | 0xf8 => Err(UnassignedCode)
  | 0xf9..=0xfa => Err(InvalidDagCbor)
  -- Floats (0xfb) are not supported
  | 0xfb => Err(InvalidDagCbor)
  | 0xfc..=0xfe => Err(UnassignedCode)
  | 0xff => Err(UnexpectedCode)
  | _ => Err(UnexpectedCode)
  ipld

def parse_u16 (bytes : ByteArray) : UInt16 :=
  (Utils.fromByteArrayBE bytes).toUInt16
  
def parse_u32 (bytes : ByteArray) : UInt32 :=
  (Utils.fromByteArrayBE bytes).toUInt32

def parse_u64 (bytes : ByteArray) : UInt64 :=
  (Utils.fromByteArrayBE bytes).toUInt64
  
def deserialize_null (self : Deserializer) : Ipld := do
  self.i := self.i + 1
  Ipld.null
  
def deserialize_bool (self : Deserializer) (bool : Bool) : Ipld := do
  self.i := self.i + 1
  Ipld.bool bool

def deserialize_u8 (self : Deserializer) : Ipld := do
  let num := self.bytes[self.i+1].toUInt64
  self.i := self.i + 2
  Ipld.number num

def deserialize_u16 (self : Deserializer) : Ipld := do
  let num := (parse_u16 self.bytes[self.i+1:self.i+3]).toUInt64
  self.i := self.i + 4
  Ipld.number num

def deserialize_u32 (self : Deserializer) : Ipld := do
  let num := (parse_u32 self.bytes[self.i+1:self.i+5]).toUInt64
  self.i := self.i + 6
  Ipld.number num

def deserialize_u64 (self : Deserializer) : Ipld := do
  let num := (parse_u64 self.bytes[self.i+1:self.i+9]).toUInt64
  self.i := self.i + 10
  Ipld.number num
  
def deserialize_bytes (self : Deserializer) (inc : UInt64) (len : UInt64) : Ipld := do
  let bytes : ByteArray := 
    if len == 0 then []
    else self.bytes[self.i+1:self.i+1+len]
  self.i := self.i + inc + len
  Ipld.byte bytes
  
def deserialize_string (self : Deserializer) (inc : UInt64) (len : UInt64) : Ipld := do
  let str : String := 
    if len == 0 then ""
    else self.bytes[self.i+1:self.i+1+len].ToString
  self.i := self.i + inc + len
  Ipld.string str
  
def deserialize_array (self : Deserializer) (inc : UInt64) (len : UInt64) : Ipld := do
  self.i := self.i + inc
  let array : Array Ipld := []
  for item in [:len] do
    array.push deserialize self
  Ipld.array array

def deserialize_map (self : Deserializer) (inc : UInt64) (len : UInt64) : Ipld := do
  self.i := self.i + inc
  let map : RBNode String (fun _ => Ipld) := RBNode.empty
  for item in [:len] do
    -- Deserialize key, must be a text string
    if self.bytes[self.i] < 0x60 || self.bytes[self.i] > 0x7b
    then return Err(UnexpectedCode)
    let key : String := (deserialize self).s
    -- Deserialize value into IPLD object
    let val : Ipld := deserialize self
    map.insert key val
  Ipld.object map

def deserialize_link (self : Deserializer) : Ipld := do
  self.i := self.i + 2
  let bytes : ByteArray := (deserialize self).byte
  if bytes[0] != 0
  then return Err(InvalidCidPrefix)
  let cid := Cid.fromBytes bytes
  Ipld.link := cid
