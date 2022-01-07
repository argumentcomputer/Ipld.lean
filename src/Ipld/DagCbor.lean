import Ipld.Ipld
import Ipld.Cid
import Ipld.Utils
import Std.Data.RBTree
import Init.Control.EState
import Init.Data.ToString
import Ipld.Multihash

open Std (RBNode RBMap)

def ser_null : ByteArray := ByteArray.mk #[0xf6]

def ser_bool : Bool -> ByteArray
| true => ByteArray.mk #[0xf5]
| false => ByteArray.mk #[0xf4]

def ser_u8 (major : UInt8) (n : UInt8) : ByteArray :=
  if n <= 0x17
  then ByteArray.mk #[((major.toNat.shiftLeft 5).lor n.toNat).toUInt8]
  else ByteArray.mk #[((major.toNat.shiftLeft 5).lor 24).toUInt8, n]

def ser_u16 (major : UInt8) (n : UInt16) : ByteArray :=
  if n <= 255
  then ser_u8 major n.toUInt8
  else
    let buf := ByteArray.mk #[((major.toNat.shiftLeft 5).lor 25).toUInt8]
    ByteArray.append buf (n.toNat.toByteArrayBE)

def ser_u32 (major : UInt8) (n : UInt32) : ByteArray :=
  if n <= 65535
  then ser_u16 major n.toUInt16
  else
    let buf := ByteArray.mk #[((major.toNat.shiftLeft 5).lor 26).toUInt8]
    ByteArray.append buf (n.toNat.toByteArrayBE)

def ser_u64 (major: UInt8) (n : UInt64) : ByteArray :=
  if n <= 4294967295
  then ser_u32 major n.toUInt32
  else
    let buf := ByteArray.mk #[((major.toNat.shiftLeft 5).lor 27).toUInt8]
    ByteArray.append buf (n.toNat.toByteArrayBE)

def ser_string (s: String) : ByteArray :=
  let str_bytes := s.toUTF8
  (ser_u64 3 str_bytes.size.toUInt64).append str_bytes

def ser_bytes (b: ByteArray) : ByteArray :=
  ByteArray.append (ser_u64 2 b.size.toUInt64) b

def ser_link(l: Cid) : ByteArray := do
  let mut out := ByteArray.mk #[]
  out := out.append (ser_u64 6 42)
  out.append (ser_bytes (ByteArray.append (ByteArray.mk #[0]) (Cid.toBytes l)))

def nodeToList (map : RBNode String (fun _ => Ipld)) : List (String × Ipld) := 
  map.revFold (fun as a b => (a,b)::as) []

-- TODO: Add termination_by measure to show that serialize does terminate
mutual
partial def serialize : Ipld -> ByteArray
  | Ipld.null => ser_null
  | Ipld.bool b => ser_bool b
  | Ipld.number n => ser_u64 0 n
  | Ipld.string s => ser_string s
  | Ipld.byte b => ser_bytes b
  | Ipld.array a => ser_array a
  | Ipld.object o => ser_object o
  | Ipld.link cid => ser_link cid

partial def ser_array (a: Array Ipld) : ByteArray := do
  let mut self := ser_u64 4 a.size.toUInt64
  for i in [:a.size] do
    self := self.append (serialize a[i])
  self

partial def ser_object (o: RBNode String (fun _ => Ipld)) : ByteArray := do
  let list := nodeToList o
  let mut self := ser_u64 5 list.length.toUInt64
  for (k, v) in list do
    self := self.append (ser_string k)
    self := self.append (serialize v)
  self
end

structure ByteCursor where
  bytes : ByteArray
  pos: Nat
  deriving Repr

inductive DeserializeError
| UnexpectedEOF
| NoAlt
| UnknownCborTag (tag: UInt8)
| UnexpectedCborCode (code: Nat)
| CidLenOutOfRange (len: UInt8)
| CidPrefix (tag: UInt8)
| CidRead
| ExpectedTag (tag: UInt8) (read: UInt8)
deriving Repr


instance : ToString ByteCursor where
  toString bc := (toString bc.bytes.data.data) ++ "[" ++ (toString bc.pos) ++ "]"

instance : ToString DeserializeError where
  toString
  | DeserializeError.UnexpectedEOF => "Unexpected EOF"
  | DeserializeError.NoAlt => "No Alt"
  | DeserializeError.UnknownCborTag t => "Unknown Tag " ++ toString t
  | DeserializeError.UnexpectedCborCode t => "UnexpectedCborCode " ++ toString t
  | DeserializeError.CidRead => "CidRead"
  | DeserializeError.ExpectedTag t r => 
    "Expected Tag " ++ toString t ++ ", read " ++ toString r
  | DeserializeError.CidLenOutOfRange len => "CidLenOutOfRange " ++ toString len
  | DeserializeError.CidPrefix tag => "CidPrefix " ++ toString tag

def getPos (x: ByteCursor) : Nat := x.pos
def setPos (x: ByteCursor) (i: Nat) : ByteCursor := 
  { bytes := x.bytes, pos := i}

def Deserializer (α : Type): Type := EStateM DeserializeError ByteCursor α

instance : Monad Deserializer where
  bind     := EStateM.bind
  pure     := EStateM.pure
  map      := EStateM.map
  seqRight := EStateM.seqRight

instance : MonadStateOf ByteCursor Deserializer where
  set       := EStateM.set
  get       := EStateM.get
  modifyGet := EStateM.modifyGet

instance : MonadExceptOf DeserializeError Deserializer where
  throw    := EStateM.throw
  tryCatch := EStateM.tryCatch

def next : Deserializer UInt8 := do
  let { bytes, pos } <- get
  if pos + 1 > bytes.size then throw DeserializeError.UnexpectedEOF
  set (ByteCursor.mk bytes (pos + 1))
  return bytes[pos]

def take (n: Nat) : Deserializer ByteArray := do
  let { bytes, pos } <- get
  if pos + n > bytes.size then throw DeserializeError.UnexpectedEOF
  set (ByteCursor.mk bytes (pos + n))
  return bytes.extract pos (pos + n)

def tag (t: UInt8) : Deserializer UInt8 := do
  let tag <- next
  if t == tag
  then return tag
  else throw (DeserializeError.ExpectedTag t tag)

def alt {α : Type} (ds : List (Deserializer α)) : Deserializer α := do
  match ds with
  | [] => throw DeserializeError.NoAlt
  | c::cs => EStateM.orElse' c (alt cs)

#eval (EStateM.run next { bytes := ByteArray.mk #[0,1,2], pos := 0 })
#eval (EStateM.run (take 3) { bytes := ByteArray.mk #[0,1,2], pos := 0 })
#eval (EStateM.run (tag 3) { bytes := ByteArray.mk #[0,1,2], pos := 0 })

def read_u8: Deserializer UInt8 := next

def read_u16: Deserializer UInt16 := do
  let bytes <- take 2
  return bytes.fromByteArrayBE.toUInt16

def read_u32: Deserializer UInt32 := do
  let bytes <- take 4
  return bytes.fromByteArrayBE.toUInt32

def read_u64: Deserializer UInt64 := do
  let bytes <- take 8
  return bytes.fromByteArrayBE.toUInt64

def read_bytes (len: Nat) : Deserializer ByteArray := take len

def read_str (len: Nat) : Deserializer String := do
  let bytes <- take len
  return String.fromUTF8Unchecked bytes

def repeat {α : Type} (len : Nat) (d : Deserializer α) : Deserializer (List α) := 
  match len with
  | 0 => return []
  | n+1 => List.cons <$> d <*> repeat n d

partial def repeat_il {α : Type} (d : Deserializer α) : Deserializer (List α) := do
  let {bytes, pos} <- get
  if bytes[pos] == 0xff
  then return []
  else List.cons <$> d <*> (repeat_il d)

def read_link : Deserializer Cid := do
  let ty <- read_u8
  if ty != 0x58 then throw (DeserializeError.UnknownCborTag ty)
  let len <- read_u8
  if len == 0 then throw (DeserializeError.CidLenOutOfRange len)
  let bytes <- (read_bytes len.toNat)
  if bytes[0] != 0 then throw (DeserializeError.CidPrefix bytes[0])
  let bytes := bytes.extract 0 bytes.size
  let cid := Cid.fromBytes bytes
  match cid with
  | Option.none => throw DeserializeError.CidRead
  | Option.some x => return x

def read_len : Nat -> Deserializer Nat
| 0x18 => UInt8.toNat <$> read_u8
| 0x19 => UInt16.toNat <$> read_u16
| 0x1a => UInt32.toNat <$> read_u32
| 0x1b => UInt64.toNat <$> read_u64
| x => if x <= 0x17
  then return x
  else throw (DeserializeError.UnexpectedCborCode x)

def decode_string : Deserializer String := do
  let major <- read_u8
  if 0x60 <= major && major <= 0x7b
  then (read_len (major.toNat - 0x60)) >>= read_str
  else throw (DeserializeError.UnexpectedCborCode major.toNat)

partial def deserialize : Deserializer Ipld := do
let major <- read_u8
match major with
| 0x18 => Ipld.number <$> UInt8.toUInt64 <$> read_u8
| 0x19 => Ipld.number <$> UInt16.toUInt64 <$> read_u16
| 0x1a => Ipld.number <$> UInt32.toUInt64 <$> read_u32
| 0x1b => Ipld.number <$> read_u64
-- Negative
-- | 0x38 => Ipld.number <$> UInt8.toUInt64 <$> read_u8
-- | 0x39 => Ipld.number <$> UInt8.toUInt64 <$> read_u8
-- | 0x3a => Ipld.number <$> UInt8.toUInt64 <$> read_u8
-- | 0x3b => Ipld.number <$> UInt8.toUInt64 <$> read_u8
| 0x9f => Ipld.array <$> Array.mk <$> repeat_il deserialize
-- StringMap
| 0xbf => do
  let list <- repeat_il ((·,·) <$> decode_string <*> deserialize)
  let map := RBMap.fromList list compare
  let node := match map with
  | ⟨leaf, _⟩ => leaf
  return Ipld.object node
| 0xd8 => do 
  let tag <- read_u8
  if tag == 42 then Ipld.link <$> read_link
  else throw (DeserializeError.UnknownCborTag tag)
| 0xf4 => return Ipld.bool false
| 0xf5 => return Ipld.bool true
| 0xf6 => return Ipld.null
| 0xf7 => return Ipld.null
| x => do
  if 0x00 <= x && x <= 0x17 then return (Ipld.number major.toUInt64)
  --if 0x20 <= x && x <= 0x37 then return (Ipld.number major.toUInt64)
  if 0x40 <= x && x <= 0x5b then do
    let len <- read_len (major.toNat - 0x40)
    let byte <- read_bytes len
    return Ipld.byte byte
  if 0x60 <= x && x <= 0x7b then do
    let len <- read_len (major.toNat - 0x60)
    let str <- read_str len
    return Ipld.string str
  if 0x80 <= x && x <= 0x9b then do
    let len <- read_len (major.toNat - 0x80)
    let arr <- repeat (major.toNat - 0x80) deserialize
    return Ipld.array (Array.mk arr)
  throw (DeserializeError.UnknownCborTag major)

#eval 0x60

#eval serialize (Ipld.byte (ByteArray.mk #[1, 2, 3]))
#eval (EStateM.run deserialize { bytes := ByteArray.mk #[67, 1, 2, 3], pos := 0 })


--partial def deserialize (bytes : ByteArray) : Ipld := do
--  let code := bytes[0]
--  match code with
--  | 0x18 => deserialize_u8 bytes[1]
--  | 0x19 => deserialize_u16 (byteSlice bytes 1 3)
--  | 0x1a => deserialize_u32 (byteSlice bytes 1 5)
--  | 0x1b => deserialize_u64 (byteSlice bytes 1 9)
--  | 0x58 => deserialize_bytes (byteSlice bytes 2 bytes.size) bytes[1].toNat
--  | 0x59 => (deserialize_bytes
--            (byteSlice bytes 3 bytes.size) (parseUInt16 (byteSlice bytes 1 3)).toNat)
--  | 0x5a => (deserialize_bytes
--            (byteSlice bytes 5 bytes.size) (parseUInt32 (byteSlice bytes 1 5)).toNat)
--  | 0x5b => (deserialize_bytes
--            (byteSlice bytes 9 bytes.size) (parseUInt64 (byteSlice bytes 1 9)).toNat)
--  | 0x78 => deserialize_string (byteSlice bytes 2 bytes.size) bytes[1].toNat
--  | 0x79 => (deserialize_string
--            (byteSlice bytes 3 bytes.size) (parseUInt16 (byteSlice bytes 1 3)).toNat)
--  | 0x7a => (deserialize_string
--            (byteSlice bytes 5 bytes.size) (parseUInt32 (byteSlice bytes 1 5)).toNat)
--  | 0x7b => (deserialize_string
--            (byteSlice bytes 9 bytes.size) (parseUInt64 (byteSlice bytes 1 9)).toNat)
--  --| 0x98 => deserialize_array (byteSlice bytes 2 bytes.size) bytes[1].toNat
--  --| 0x99 => (deserialize_array
--  --          (byteSlice bytes 3 bytes.size) (parseUInt16 (byteSlice bytes 1 3)).toNat)
--  --| 0x9a => (deserialize_array
--  --          (byteSlice bytes 5 bytes.size) (parseUInt32 (byteSlice bytes 1 5)).toNat)
--  --| 0x9b => (deserialize_array
--  --          (byteSlice bytes 9 bytes.size) (parseUInt64 (byteSlice bytes 1 9)).toNat)
--  --| 0xb8 => deserialize_map (byteSlice bytes 2 bytes.size) bytes[1].toNat
--  --| 0xb9 => (deserialize_map
--  --          (byteSlice bytes 3 bytes.size) (parseUInt16 (byteSlice bytes 1 3)).toNat)
--  --| 0xba => (deserialize_map
--  --          (byteSlice bytes 5 bytes.size) (parseUInt32 (byteSlice bytes 1 5)).toNat)
--  --| 0xbb => (deserialize_map
--  --          (byteSlice bytes 9 bytes.size) (parseUInt64 (byteSlice bytes 1 9)).toNat)
--  | 0xd8 =>
--    if bytes[1] == 0x2a
--    then
--      match deserialize (byteSlice bytes 2 bytes.size) with
--      | Ipld.byte b => deserialize_link b
--      | _ => Ipld.link (Cid.fromBytes { data := #[] }).get!
--    else Ipld.link (Cid.fromBytes { data := #[] }).get!
--  | 0xf4 => Ipld.bool Bool.false
--  | 0xf5 => Ipld.bool Bool.true
--  | 0xf6 => Ipld.null
--  | 0xf7 => Ipld.null
--  | x =>
--    if x <= 0x17 then
--      Ipld.number code.toUInt64
--    else if x >= 0x40 && x <= 0x57 then
--      deserialize_bytes (byteSlice bytes 1 bytes.size) (code - 0x40).toNat
--    else if x >= 0x60 && x <= 0x77 then
--      deserialize_string (byteSlice bytes 1 bytes.size) (code - 0x60).toNat
--    --else if x >= 0x80 && x <= 0x97 then
--    --  deserialize_array (byteSlice bytes 1 bytes.size) (code - 0x80).toNat
--    --else if x >= 0xb0 && x <= 0xb7 then
--    --  deserialize_map (byteSlice bytes 1 bytes.size) (code - 0xa0).toNat
--    else
--      Ipld.null

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
  
--end

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

#eval serialize Ipld.null == { data := #[0xf6] }
#eval serialize (Ipld.bool true) == { data := #[0xf5] }
#eval serialize (Ipld.number 1) == { data := #[0x1] }
#eval serialize (Ipld.string "Hello") == string_ex1_encoded
#eval serialize (Ipld.byte "Hello".toUTF8) == bytes_ex1_encoded
#eval serialize (Ipld.array array_ex1) == array_ex1_encoded
#eval serialize (Ipld.object object_ex1) == object_ex1_encoded
#eval serialize (Ipld.link cid_ex1) == cid_ex1_encoded


--#eval deserialize { data := #[0xf6] } == Ipld.null
--#eval deserialize { data := #[0xf5] } == Ipld.bool true
--#eval deserialize { data := #[0x1] } == Ipld.number 1
--#eval deserialize bytes_ex1_encoded == Ipld.byte "Hello".toUTF8

--#eval (deserialize string_ex1_encoded)
--#eval deserialize array_ex1_encoded == Ipld.array Ser.Test.array_ex1
--#eval deserialize object_ex1_encoded == Ipld.object Ser.Test.object_ex1
--#eval deserialize cid_ex1_encoded == Ipld.link Ser.Test.cid_ex1

--Roundtrip testing
--#eval De.deserialize (serialize Ipld.null) == Ipld.null
--#eval De.deserialize (serialize Ipld.bool true) == Ipld.bool true
--#eval De.deserialize (serialize Ipld.number 1) == Ipld.number 1
--#eval De.deserialize (serialize Ipld.string "Hello") == Ipld.string "Hello"
--#eval De.deserialize (serialize Ipld.byte "Hello".toUTF8) == Ipld.byte "Hello".toUTF8
--#eval De.deserialize (serialize Ipld.array array_ex1) == Ipld.array array_ex1
--#eval De.deserialize (serialize Ipld.object object_ex1) == Ipld.object object_ex1
--#eval De.deserialize (serialize Ipld.cid cid_ex1) == Ipld.cid cid_ex1

end Test

