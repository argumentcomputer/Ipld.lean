use crate::{
  multihash::Multihash,
  unsigned_varint::{
    to_varint,
    varint_read_u64,
  },
};
use anyhow::Result;
use serde::{
  de,
  ser,
};
use serde_bytes::ByteBuf;

use std::{
  convert::TryFrom,
  fmt,
  io::Read,
};

/// Supported CID spec:
/// dag-cbor codec
/// SHA-256 or Keccak

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Cid {
  pub version: u64,
  pub codec: u64,
  pub hash: Multihash,
}

impl Cid {
  pub fn new(version: u64, codec: u64, hash: Multihash) -> Self {
    Self { version, codec, hash }
  }

  pub fn to_bytes(&self) -> Vec<u8> {
    let mut bytes = vec![];
    bytes.extend(to_varint(self.version));
    bytes.extend(to_varint(self.codec));
    bytes.extend(self.hash.to_bytes());
    bytes
  }

  pub fn from_bytes<R: Read>(r: &mut R) -> Result<Cid> {
    let version = varint_read_u64(r).unwrap();
    let codec = varint_read_u64(r).unwrap();
    let hash = Multihash::from_bytes(r).unwrap();
    Ok(Cid { version, codec, hash })
  }
}

impl fmt::Display for Cid {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}, {}, {}", self.version, self.codec, self.hash)
  }
}

impl ser::Serialize for Cid {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where S: ser::Serializer {
    let value = ByteBuf::from(self.to_bytes());
    serializer.serialize_newtype_struct(CID_SERDE_PRIVATE_IDENTIFIER, &value)
  }
}

impl TryFrom<Vec<u8>> for Cid {
  type Error = anyhow::Error;

  fn try_from(bytes: Vec<u8>) -> Result<Self, Self::Error> {
    Self::from_bytes(&mut &bytes[..])
  }
}

impl TryFrom<&[u8]> for Cid {
  type Error = anyhow::Error;

  fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
    Self::from_bytes(&mut &bytes[..])
  }
}

pub const CID_SERDE_PRIVATE_IDENTIFIER: &str =
  "$__private__serde__identifier__for__cid";

pub struct BytesToCidVisitor;

impl<'de> de::Visitor<'de> for BytesToCidVisitor {
  type Value = Cid;

  fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    write!(fmt, "a valid CID in bytes")
  }

  fn visit_bytes<E>(self, value: &[u8]) -> Result<Self::Value, E>
  where E: de::Error {
    Cid::try_from(value).map_err(|err| {
      de::Error::custom(format!("Failed to deserialize CID: {:?}", err))
    })
  }

  /// Some Serde data formats interpret a byte stream as a sequence of bytes
  /// (e.g. `serde_json`).
  fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
  where A: de::SeqAccess<'de> {
    let mut bytes = Vec::new();
    while let Some(byte) = seq.next_element()? {
      bytes.push(byte);
    }
    Cid::try_from(bytes).map_err(|err| {
      de::Error::custom(format!("Failed to deserialize CID: {:?}", err))
    })
  }
}

#[cfg(test)]
mod tests {
  use crate::{
    cid::Cid,
    multihash::Multihash,
  };

  #[test]
  fn cid_bytes_roundrip() {
    let data = vec![1];
    let digest = Multihash::sha3_256(&data);
    let cid = Cid { version: 0x01, codec: 0x71, hash: digest };
    assert_eq!(cid, Cid::from_bytes(&mut &cid.to_bytes()[..]).unwrap());
  }
}
