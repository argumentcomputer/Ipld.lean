-- TODO:
-- Serialize contents of object, not just length
-- Fix errors
-- Add error handling
-- Find out how to get the max of an integer
-- Potentially Add int map object

import Ipld.Ipld
import Std.Data.RBTree

open Std (RBNode)

def serialize (ipld: Ipld) : ByteArray
  | Ipld.null => serialize_null
  | Ipld.bool b => serialize_bool b
  | Ipld::number n => serialize_u64 n
  | Ipld::string s => serialize_string s
  | Ipld::byte b => serialize_bytes b
  | Ipld::array e => serialize_array e 
  | Ipld::object o => serialize_object o
  | Ipld::link cid => serialize_link cid

--{ data := #[0x00, 0x00]} : ByteArray
def serialize_null : ByteArray := 
  { data := #[0xf6] } 
  
def serialize_bool : Bool -> ByteArray
  | true => { data := #[0xf5] }
  | false => { data := #[0xf4] }

def serialize_u8 (major: UInt8, n : UInt8) : ByteArray :=
  if n <= 0x17
  then { data := #[major << 5 | n] }
  else { data := #[major << 5 | 24, n] }
  
def serialize_u16 (major: UInt8, n : UInt16) : ByteArray :=
  if n <= UInt8.max
  then serialize_u8 major n.toUInt8
  else
    let buf := { data := #[major << 5 | 25, 0, 0] }
    let bytes := n.to_be_bytes
    -- Copy all of bytes into buf starting at buf[1]
    bytes.copySlice(0, buf, 1, true)

def serialize_u32 (major: UInt8, n : UInt32) : ByteArray :=
  if n < UInt16.max
  then serialize_u16 major n.toUInt16
  else
    let buf := { data := #[major << 5 | 26, 0, 0, 0, 0] }
    let bytes := n.to_be_bytes
    bytes.copySlice(0, buf, 1, true)

def serialize_u64 (major: UInt8, n : UInt64) : ByteArray :=
  if n < UInt32.max
  then serialize_u32 major n.toUInt32
  else
    let buf := { data := #[major << 5 | 27, 0, 0, 0, 0, 0, 0, 0, 0] }
    let bytes := n.to_be_bytes
    bytes.copySlice(0, buf, 1, true)

def serialize_string (s: String) : ByteArray :=
  serialize_u64 3 s.len.toUInt64
  s.toByteArray

def serialize_bytes (b: ByteArray) : ByteArray :=
  serialize_u64 2 b.len.toUInt64
  b

def serialize_array (a: Array) : ByteArray :=
  for i in a
    serialize i

def serialize_object (o: object) : ByteArray
  serialize_u64 5 o.len.toUInt64
   
def serialize_link (l: link) : ByteArray
  serialize_u64 6 l
 
