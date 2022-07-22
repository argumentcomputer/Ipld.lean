import Ipld.Ipld
import Ipld.Multihash

namespace DagCbor

def serNull : ByteArray :=
  ⟨#[0xf6]⟩

def serBool : Bool → ByteArray
  | true  => ⟨#[0xf5]⟩
  | false => ⟨#[0xf4]⟩

def serU8 (major : UInt8) (n : UInt8) : ByteArray :=
  if n <= 0x17
  then ⟨#[((major.toNat.shiftLeft 5).lor n.toNat).toUInt8]⟩
  else ⟨#[((major.toNat.shiftLeft 5).lor 24).toUInt8, n]⟩

def serU16 (major : UInt8) (n : UInt16) : ByteArray :=
  if n <= 255
  then serU8 major n.toUInt8
  else
    let maj := ((major.toNat.shiftLeft 5).lor 25).toUInt8
    let buf := ByteArray.mk #[maj, 0, 0]
    let num := (n.toNat.toByteArrayBE)
    ByteArray.copySlice num 0 buf (buf.size - num.size) 2

def serU32 (major : UInt8) (n : UInt32) : ByteArray :=
  if n <= 65535
  then serU16 major n.toUInt16
  else
    let maj := ((major.toNat.shiftLeft 5).lor 26).toUInt8
    let buf := ByteArray.mk #[maj, 0, 0, 0, 0]
    let num := (n.toNat.toByteArrayBE)
    ByteArray.copySlice num 0 buf (buf.size - num.size) 4

def serU64 (major: UInt8) (n : UInt64) : ByteArray :=
  if n <= 4294967295
  then serU32 major n.toUInt32
  else
    let maj : UInt8 := ((major.toNat.shiftLeft 5).lor 27).toUInt8
    let buf := ByteArray.mk #[maj, 0, 0, 0, 0, 0, 0, 0, 0]
    let num := (n.toNat.toByteArrayBE)
    ByteArray.copySlice num 0 buf (buf.size - num.size) 8

def serString (s: String) : ByteArray :=
  let str_bytes := s.toUTF8
  (serU64 3 str_bytes.size.toUInt64).append str_bytes

def serBytes (b: ByteArray) : ByteArray :=
  ByteArray.append (serU64 2 b.size.toUInt64) b

def serLink (l: Cid) : ByteArray :=
  let buf := Cid.toBytes l
  (serU64 6 42) ++ serU64 2 (buf.size.toUInt64 + 1) ++ ⟨#[0]⟩ ++ buf

def nodeToList (map : Std.RBNode String (fun _ => Ipld)) :
    List (String × Ipld) := 
  map.revFold (fun as a b => (a,b)::as) []

-- TODO: Add termination_by measure to show that serialize does terminate
mutual

  partial def serialize : Ipld → ByteArray
    | Ipld.null     => serNull
    | Ipld.bool   b => serBool b
    | Ipld.number n => serU64 0 n
    | Ipld.string s => serString s
    | Ipld.bytes  b => serBytes b
    | Ipld.array  a => serArray a
    | Ipld.object o => serObject o
    | Ipld.link cid => serLink cid

  partial def serArray (as: Array Ipld) : ByteArray :=
    as.foldl (init := serU64 4 as.size.toUInt64)
      fun acc a => acc ++ serialize a

  partial def serObject (o : Std.RBNode String (fun _ => Ipld)) : ByteArray :=
    let list := nodeToList o
    list.foldl (init := serU64 5 list.length.toUInt64)
      fun acc (k, v) => acc ++ serString k ++ serialize v

end

structure ByteCursor where
  bytes : ByteArray
  pos: Nat
  deriving Repr

inductive DeserializeError
  | UnexpectedEOF
  | NoAlt
  | UnknownCborTag (tag : UInt8)
  | UnexpectedCborCode (code : Nat)
  | CidLenOutOfRange (len : UInt8)
  | CidPrefix (tag : UInt8)
  | CidRead
  | ExpectedTag (tag : UInt8) (read : UInt8)
  deriving BEq, Repr

instance : ToString ByteCursor where
  toString bc := (toString bc.bytes.data.data) ++ "[" ++ (toString bc.pos) ++ "]"

instance : ToString DeserializeError where toString
  | .UnexpectedEOF => "Unexpected EOF"
  | .NoAlt => "No Alt"
  | .UnknownCborTag t => "Unknown Tag " ++ toString t
  | .UnexpectedCborCode t => "UnexpectedCborCode " ++ toString t
  | .CidRead => "CidRead"
  | .ExpectedTag t r => "Expected Tag " ++ toString t ++ ", read " ++ toString r
  | .CidLenOutOfRange len => "CidLenOutOfRange " ++ toString len
  | .CidPrefix tag => "CidPrefix " ++ toString tag

def getPos (x: ByteCursor) : Nat :=
  x.pos

def setPos (x: ByteCursor) (i: Nat) : ByteCursor := 
  { bytes := x.bytes, pos := i}

abbrev Deserializer (α) := EStateM DeserializeError ByteCursor α

def next : Deserializer UInt8 := do
  let { bytes, pos } ← get
  if pos + 1 > bytes.size then throw DeserializeError.UnexpectedEOF
  set (ByteCursor.mk bytes (pos + 1))
  return bytes[pos]!

def take (n: Nat) : Deserializer ByteArray := do
  let { bytes, pos } ← get
  if pos + n > bytes.size then throw DeserializeError.UnexpectedEOF
  set (ByteCursor.mk bytes (pos + n))
  return bytes.extract pos (pos + n)

def tag (t: UInt8) : Deserializer UInt8 := do
  let tag ← next
  if t == tag
  then return tag
  else throw (DeserializeError.ExpectedTag t tag)

def alt (ds : List (Deserializer α)) : Deserializer α := do
  match ds with
  | []      => throw DeserializeError.NoAlt
  | c :: cs => EStateM.orElse' c (alt cs)

def readU8 : Deserializer UInt8 :=
  next

def readU16 : Deserializer UInt16 := do
  let bytes ← take 2
  return bytes.asBEtoNat.toUInt16

def readU32 : Deserializer UInt32 := do
  let bytes ← take 4
  return bytes.asBEtoNat.toUInt32

def readU64 : Deserializer UInt64 := do
  let bytes ← take 8
  return bytes.asBEtoNat.toUInt64

def readBytes (len: Nat) : Deserializer ByteArray :=
  take len

def readString (len: Nat) : Deserializer String := do
  let bytes ← take len
  return String.fromUTF8Unchecked bytes

def repeatFor (len : Nat) (d : Deserializer α) : Deserializer (List α) := 
  match len with
  | 0     => return []
  | n + 1 => List.cons <$> d <*> repeatFor n d

partial def repeatIl (d : Deserializer α) : Deserializer (List α) := do
  let {bytes, pos} ← get
  if bytes[pos]! == 0xff
    then return []
    else List.cons <$> d <*> (repeatIl d)

def readLink : Deserializer Cid := do
  let ty ← readU8
  if ty != 0x58 then throw (DeserializeError.UnknownCborTag ty)
  let len ← readU8
  if len == 0 then throw (DeserializeError.CidLenOutOfRange len)
  let bytes ← (readBytes len.toNat)
  if bytes[0]! != 0 then throw (DeserializeError.CidPrefix bytes[0]!)
  let bytes := bytes.extract 1 bytes.size
  let cid := Cid.fromBytes bytes
  match cid with
  | some x => return x
  | none   => throw DeserializeError.CidRead

def readLen : Nat → Deserializer Nat
  | 0x18 => UInt8.toNat  <$> readU8
  | 0x19 => UInt16.toNat <$> readU16
  | 0x1a => UInt32.toNat <$> readU32
  | 0x1b => UInt64.toNat <$> readU64
  | x => if x <= 0x17
    then return x
    else throw (DeserializeError.UnexpectedCborCode x)

def decodeString : Deserializer String := do
  let major ← readU8
  if 0x60 <= major && major <= 0x7b
  then (readLen (major.toNat - 0x60)) >>= readString
  else throw (DeserializeError.UnexpectedCborCode major.toNat)

partial def deserializeIpld : Deserializer Ipld := do
  let major ← readU8
  match major with
  | 0x18 => .number <$> UInt8.toUInt64  <$> readU8
  | 0x19 => .number <$> UInt16.toUInt64 <$> readU16
  | 0x1a => .number <$> UInt32.toUInt64 <$> readU32
  | 0x1b => .number <$> readU64
  -- Negative
  -- | 0x38 => Ipld.number <$> UInt8.toUInt64 <$> read_u8
  -- | 0x39 => Ipld.number <$> UInt8.toUInt64 <$> read_u8
  -- | 0x3a => Ipld.number <$> UInt8.toUInt64 <$> read_u8
  -- | 0x3b => Ipld.number <$> UInt8.toUInt64 <$> read_u8
  -- Major type 4: array
  | 0x9f => .array <$> Array.mk <$> repeatIl deserializeIpld
  -- StringMap
  -- Major type 5: map of pairs
  | 0xbf =>
    let list ← repeatIl ((·,·) <$> decodeString <*> deserializeIpld)
    return Ipld.mkObject list
  -- Major type 6: tag
  | 0xd8 =>
    let tag ← readU8
    if tag == 42 then Ipld.link <$> readLink
    else throw (DeserializeError.UnknownCborTag tag)
  | 0xf4 => return .bool false
  | 0xf5 => return .bool true
  | 0xf6 => return .null
  | 0xf7 => return .null
  | x =>
    -- Major type 0: unsigned integer
    if 0x00 <= x && x <= 0x17 then return .number major.toUInt64
    -- Major type 1: negative integer
    --if 0x20 <= x && x <= 0x37 then return (Ipld.number major.toUInt64)
    -- Major type 2: byte string
    if 0x40 <= x && x <= 0x5b then
      let len ← readLen (major.toNat - 0x40)
      let bytes ← readBytes len
      return .bytes bytes
    -- Major type 3: text string
    if 0x60 <= x && x <= 0x7b then
      let len ← readLen (major.toNat - 0x60)
      let str ← readString len
      return .string str
    -- Major type 4: array
    if 0x80 <= x && x <= 0x9b then
      let len ← readLen (major.toNat - 0x80)
      let arr ← repeatFor len deserializeIpld
      return .array (Array.mk arr)
    -- Major type 5: map
    if 0xa0 <= x && x <= 0xbb then
      let len ← readLen (major.toNat - 0xa0)
      let list ← repeatFor len ((·,·) <$> decodeString <*> deserializeIpld)
      return .mkObject list
    throw (DeserializeError.UnknownCborTag major)

def deserialize (x: ByteArray) : Except DeserializeError Ipld :=
  match EStateM.run deserializeIpld (ByteCursor.mk x 0) with
  | EStateM.Result.ok    x _ => Except.ok x
  | EStateM.Result.error e _ => Except.error e

end DagCbor
