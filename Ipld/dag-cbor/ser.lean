-- TODO:
-- Serialize contents of object, not just length
-- Fix errors
-- Add error handling
-- Find out how to get the max of an integer
-- Potentially Add int map object

import Ipld.Ipld
import Std.Data.RBTree

open Std (RBNode)

structure Serializer where
  bytes : ByteArray
  --i : UInt64
     
def serialize (self : Serializer) (ipld : Ipld) : Serializer :=
  match ipld with
  | Ipld.null => serialize_null self
  | Ipld.bool b => serialize_bool self b
  | Ipld.number n => serialize_u64 self n
  | Ipld.string s => serialize_string self s
  | Ipld.byte b => serialize_bytes self b
  | Ipld.array e => serialize_array self e 
  | Ipld.object o => serialize_object self o
  | Ipld.link cid => serialize_link self cid

def serialize_bool (self : Serializer) (bool : Bool) : Serializer :=
  match bool with
  | true => self.bytes.push 0xf5
  | false => self.bytes.push 0xf4

def serialize_u8 (self : Serializer) (major : UInt8) (len : UInt8) : Serializer :=
  if len <= 0x17
  then self.bytes.push ((major.toNat.shiftLeft 5).lor len.toNat).toUInt8
  else self.bytes.append { data := #[((major.toNat.shiftLeft 5).lor 24).toUInt8, len] }
  
def serialize_u16 (self : Serializer) (major : UInt8) (len : UInt16) : Serializer :=
  --if len <= UInt8.max
  if len <= 255
  then serialize_u8 self major len.toUInt8
  else
    let buf : ByteArray := { data := #[((major.toNat.shiftLeft 5).lor 25).toUInt8] }
    let bytes : ByteArray := Utils.toByteArrayBE len.toNat
    buf.append bytes
    self.bytes.append buf


def serialize_u32 (self : Serializer) (major: UInt8) (len : UInt32) : Serializer :=
  --if len <= UInt16.max
  if len <= 65535
  then serialize_u16 self major len.toUInt16
  else
    let buf := { data := #[((major.toNat.shiftLeft 5).lor 26).toUInt8] }
    let bytes := toByteArrayBE len.toNat
    buf.append bytes
    self.bytes.append buf

def serialize_u64 (self : Serializer) (major: UInt8) (len : UInt64) : Serializer :=
  --if len < UInt32.max
  if len <= 4294967295
  then serialize_u32 self major len.toUInt32
  else do
    let buf := { data := #[((major.toNat.shiftLeft 5).lor 27).toUInt8] }
    let bytes := toByteArrayBE len.toNat
    buf.append bytes
    self.bytes.append buf

def serialize_string (self : Serializer) (s: String) : Serializer := do
  serialize_u64 self 3 s.length.toUInt64
  self.bytes.append s.toUTF8

def serialize_bytes (self : Serializer) (b: ByteArray) : Serializer := do
  serialize_u64 self 2 b.length.toUInt64
  self.bytes.append b

def serialize_array (self : Serializer) (a: Array) : Serializer := do
  serialize_u64 self 4 a.length.toUInt64
  for i in a do
    serialize self i

def serialize_object (self : Serializer) (o: RBNode String (fun _ => Ipld)) : Serializer := do
  serialize_u64 self 5 o.size.toUInt64
  for i in o do
    serialize_string self i.0
    serialize self i.1
        

def serialize_link (self : Serializer) (cid: link) : Serializer := do
  -- Write the tag
  serialize_u64 self 6 42
  let version := cid.version.toByteArray
  let len := cid.version.toByteArrayBE.length + cid.codec.toByteArrayBE.length
          + cid.hash.toBytes.len
  -- Write the array type
  serialize_u64 self 2 l
  -- Write the Cid array
 
