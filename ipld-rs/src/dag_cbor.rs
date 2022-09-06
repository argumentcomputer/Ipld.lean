use anyhow::Result;
use std::{
  collections::BTreeMap,
  io::Read,
};
use thiserror::Error;

use crate::{
  cid::Cid,
  ipld::Ipld,
};

pub fn serialize(ipld: &Ipld) -> Vec<u8> {
  match ipld {
    Ipld::Null => ser_null(),
    Ipld::Bool(b) => ser_bool(b),
    Ipld::Number(i) => ser_u64(0, *i),
    Ipld::String(s) => ser_string(s),
    Ipld::Bytes(b) => ser_bytes(b),
    Ipld::Array(a) => ser_array(a),
    Ipld::Object(o) => ser_object(o),
    Ipld::Link(c) => ser_link(c),
  }
}

fn ser_null() -> Vec<u8> { vec![0xf6] }

fn ser_bool(b: &bool) -> Vec<u8> {
  match b {
    true => vec![0xf5],
    false => vec![0xf4],
  }
}

fn ser_u8(major: u8, n: u8) -> Vec<u8> {
  if n <= 0x17 { vec![(major << 5) | n] } else { vec![(major << 5) | 24, n] }
}

fn ser_u16(major: u8, n: u16) -> Vec<u8> {
  if n <= 255 {
    ser_u8(major, u8::try_from(n).unwrap())
  }
  else {
    let mut buf = vec![(major << 5) | 25];
    buf.extend(n.to_be_bytes());
    buf
  }
}

fn ser_u32(major: u8, n: u32) -> Vec<u8> {
  if n <= 65535 {
    ser_u16(major, u16::try_from(n).unwrap())
  }
  else {
    let mut buf = vec![(major << 5) | 26];
    buf.extend(n.to_be_bytes());
    buf
  }
}

fn ser_u64(major: u8, n: u64) -> Vec<u8> {
  if n <= 4294967295 {
    ser_u32(major, u32::try_from(n).unwrap())
  }
  else {
    let mut buf = vec![(major << 5) | 27];
    buf.extend(n.to_be_bytes());
    buf
  }
}

fn ser_string(s: &String) -> Vec<u8> {
  let str_bytes = s.as_bytes();
  let mut result = ser_u64(3, str_bytes.len() as u64);
  result.extend(str_bytes);
  result
}

fn ser_bytes(b: &Vec<u8>) -> Vec<u8> {
  let mut result = ser_u64(2, b.len() as u64);
  result.extend(b);
  result
}

fn ser_link(l: &Cid) -> Vec<u8> {
  let buf = l.to_bytes();
  let mut result = ser_u64(6, 42);
  result.extend(ser_u64(2, (buf.len() + 1) as u64));
  result.push(0);
  result.extend(buf);
  result
}

fn ser_array(a: &Vec<Ipld>) -> Vec<u8> {
  let mut result = ser_u64(4, a.len() as u64);
  for ipld in a {
    result.extend(serialize(ipld));
  }
  result
}

fn ser_object(m: &BTreeMap<String, Ipld>) -> Vec<u8> {
  let mut result = ser_u64(5, m.len() as u64);
  for (key, val) in m.iter() {
    result.extend(ser_string(key));
    result.extend(serialize(val));
  }
  result
}

// TODO: Move this to error.rs
#[derive(Error, Debug)]
enum DeserializeError {
  //#[error("Unexpected CBOR tag: `{0}`")]
  // UnknownCborTag (tag: u8),
  #[error("Unknown CBOR tag")]
  UnknownCborTag,
  #[error("Unexpected CBOR code")]
  UnexpectedCborCode,
  #[error("CID length too large")]
  CidLenOutofRange,
  #[error("Invalid CID prefix")]
  CidPrefix,
  #[error("Failed to read CID")]
  CidRead,
}

use crate::dag_cbor::DeserializeError::*;

pub fn deserialize<R: Read>(r: &mut R) -> Result<Ipld> {
  let major = read_u8(r)?;
  match major {
    0x00..=0x17 => Ok(Ipld::Number(major as u64)),
    0x18 => Ok(Ipld::Number(read_u8(r)? as u64)),
    0x19 => Ok(Ipld::Number(read_u16(r)? as u64)),
    0x1a => Ok(Ipld::Number(read_u32(r)? as u64)),
    0x1b => Ok(Ipld::Number(read_u64(r)?)),
    // Major type 2: Byte string
    0x40..=0x5b => {
      let len = read_len(r, major - 0x40)?;
      Ok(Ipld::Bytes(read_bytes(r, len)?))
    }
    // Major type 3: Text string
    0x60..=0x7b => {
      let len = read_len(r, major - 0x60)?;
      Ok(Ipld::String(read_string(r, len)?))
    }
    // Major type 4: Array
    0x80..=0x9b => {
      let len = read_len(r, major - 0x80)?;
      let mut arr = vec![];
      for _ in 0..len {
        if let Ok(ipld) = deserialize(r) {
          arr.push(ipld);
        }
        else {
          return Err(UnknownCborTag.into());
        }
      }
      Ok(Ipld::Array(arr))
    }
    // Major type 5: Map
    0xa0..=0xbb => {
      let len = read_len(r, major - 0xa0)?;
      let mut map: BTreeMap<String, Ipld> = BTreeMap::new();
      for _ in 0..len {
        let major = read_u8(r)?;
        let len = read_len(r, major - 0x60)?;
        let key = read_string(r, len)?;
        if let Ok(val) = deserialize(r) {
          map.insert(key, val);
        }
        else {
          return Err(UnknownCborTag.into());
        }
      }
      Ok(Ipld::Object(map))
    }
    // Map of pairs?
    // 0xbf => {},
    // Major type 6: CID Tag
    0xd8 => {
      let tag = read_u8(r)?;
      if tag == 42 {
        Ok(Ipld::Link(read_link(r)?))
      }
      else {
        Err(UnknownCborTag.into())
      }
    }
    // Major type 1: Bool
    0xf4 => Ok(Ipld::Bool(false)),
    0xf5 => Ok(Ipld::Bool(true)),
    // Major type 0: Null
    0xf6..=0xf7 => Ok(Ipld::Null),
    _ => Err(UnknownCborTag.into()),
  }
}

fn read_len<R: Read>(r: &mut R, len: u8) -> Result<u64> {
  match len {
    0x00..=0x17 => Ok(len as u64),
    0x18 => read_u8(r).map(|x| x as u64),
    0x19 => read_u16(r).map(|x| x as u64),
    0x1a => read_u32(r).map(|x| x as u64),
    0x1b => read_u64(r),
    _ => Err(UnexpectedCborCode.into()),
  }
}

fn read_u8<R: Read>(r: &mut R) -> Result<u8> {
  let mut buf = [0; 1];
  r.read_exact(&mut buf)?;
  Ok(buf[0])
}

fn read_u16<R: Read>(r: &mut R) -> Result<u16> {
  let mut buf = [0; 2];
  r.read_exact(&mut buf)?;
  Ok(u16::from_be_bytes(buf))
}

fn read_u32<R: Read>(r: &mut R) -> Result<u32> {
  let mut buf = [0; 4];
  r.read_exact(&mut buf)?;
  Ok(u32::from_be_bytes(buf))
}

fn read_u64<R: Read>(r: &mut R) -> Result<u64> {
  let mut buf = [0; 8];
  r.read_exact(&mut buf)?;
  Ok(u64::from_be_bytes(buf))
}

fn read_bytes<R: Read>(r: &mut R, len: u64) -> Result<Vec<u8>> {
  let mut buf = vec![0; len as usize];
  r.read_exact(&mut buf)?;
  Ok(buf)
}

fn read_string<R: Read>(r: &mut R, len: u64) -> Result<String> {
  let bytes = read_bytes(r, len)?;
  Ok(String::from_utf8(bytes)?)
}

fn read_link<R: Read>(r: &mut R) -> Result<Cid> {
  let ty = read_u8(r)?;
  if ty != 0x58 {
    return Err(UnknownCborTag.into());
  }
  let len = read_u8(r)? as u64;
  if len == 0 {
    return Err(CidLenOutofRange.into());
  }
  let bytes = read_bytes(r, len)?;
  if bytes[0] != 0 {
    return Err(CidPrefix.into());
  }
  Cid::from_bytes(&mut &bytes[..])
}

#[cfg(test)]
mod tests {
  use crate::{
    cid::Cid,
    dag_cbor::{
      deserialize,
      serialize,
    },
    ipld::Ipld,
    multihash::Multihash,
  };

  #[test]
  fn serde_roundtrip() {
    let ipld_null = Ipld::Null;
    let ipld_bool = Ipld::Bool(true);
    let ipld_number = Ipld::Number(0x17);
    let ipld_number_big = Ipld::Number(0x10000);
    let ipld_string = Ipld::String("Hello".into());
    let ipld_bytes = Ipld::Bytes(vec![0, 8, 4, 0]);
    let ipld_array = Ipld::Array(vec![Ipld::String("Hello".into())]);
    let ipld_object =
      Ipld::to_object(vec![("Hello".into(), Ipld::String("World".into()))]);
    let cid = Cid::new(1, 0x71, Multihash::sha3_256(&serialize(&ipld_null)));
    let _ipld_link = Ipld::Link(cid);
    // assert_eq!(serialize(&ipld_null), vec![0xf6]);
    // assert_eq!(serialize(&ipld_bool), vec![0xf5]);
    // assert_eq!(serialize(&ipld_number), vec![23]);
    // assert_eq!(serialize(&ipld_number_big), vec![0x1a, 0x00, 0x01, 0x00,
    // 0x00]); assert_eq!(serialize(&ipld_string), vec![101, 72, 101, 108,
    // 108, 111]); assert_eq!(serialize(&ipld_bytes), vec![68, 0, 8, 4, 0]);
    // assert_eq!(serialize(&ipld_array), vec![129, 101, 72, 101, 108, 108,
    // 111]); assert_eq!(serialize(&ipld_object), vec![
    //  161, 101, 72, 101, 108, 108, 111, 101, 87, 111, 114, 108, 100
    //]);
    // assert_eq!(serialize(&ipld_link), vec![
    //  216, 42, 88, 37, 0, 1, 113, 22, 32, 69, 122, 165, 228, 28, 115, 252,
    // 178,  178, 165, 119, 247, 73, 0, 207, 105, 172, 208, 72, 59, 220, 98,
    // 86, 108,  23, 111, 21, 55, 76, 252, 185, 161
    //]);
    assert_eq!(
      ipld_null,
      deserialize(&mut &serialize(&ipld_null)[..]).unwrap()
    );
    assert_eq!(
      ipld_bool,
      deserialize(&mut &serialize(&ipld_bool)[..]).unwrap()
    );
    assert_eq!(
      ipld_number,
      deserialize(&mut &serialize(&ipld_number)[..]).unwrap()
    );
    assert_eq!(
      ipld_number_big,
      deserialize(&mut &serialize(&ipld_number_big)[..]).unwrap()
    );
    assert_eq!(
      ipld_string,
      deserialize(&mut &serialize(&ipld_string)[..]).unwrap()
    );
    assert_eq!(
      ipld_bytes,
      deserialize(&mut &serialize(&ipld_bytes)[..]).unwrap()
    );
    assert_eq!(
      ipld_array,
      deserialize(&mut &serialize(&ipld_array)[..]).unwrap()
    );
    assert_eq!(
      ipld_object,
      deserialize(&mut &serialize(&ipld_object)[..]).unwrap()
    );
    // TODO: Fix Cid::from_bytes bug
    // assert_eq!(ipld_link, deserialize(&mut
    // &serialize(&ipld_link)[..]).unwrap());
  }
}
