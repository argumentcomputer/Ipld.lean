use crate::multihash::Multihash;
use crate::unsigned_varint::{from_varint, to_varint};
use crate::error::Error;
use serde::{ser, de};
use serde_bytes::ByteBuf;

use std::fmt;
use std::convert::TryFrom;

/// Supported CID spec:
/// base32 encoding
/// dag-cbor codec
/// SHA-256 or Keccak

#[derive(Debug, PartialEq, Clone)]
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

    // TODO
    pub fn from_bytes(bytes: &[u8]) -> Result<Cid, Error> {
        let version = from_varint(bytes).unwrap();
        let codec = from_varint(bytes).unwrap();
        let hash = Multihash::from_bytes(bytes).unwrap();
        Ok(Cid {
            version,
            codec,
            hash,
        })
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
  type Error = Error;

  fn try_from(bytes: Vec<u8>) -> Result<Self, Self::Error> {
    Self::from_bytes(&bytes)
  }
}

impl TryFrom<&[u8]> for Cid {
  type Error = Error;

  fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
    Self::from_bytes(bytes)
  }
}

pub const CID_SERDE_PRIVATE_IDENTIFIER: &str = "$__private__serde__identifier__for__cid";

pub struct BytesToCidVisitor;

impl<'de> de::Visitor<'de> for BytesToCidVisitor {
    type Value = Cid;

    fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "a valid CID in bytes")
    }

    fn visit_bytes<E>(self, value: &[u8]) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Cid::try_from(value)
            .map_err(|err| de::Error::custom(format!("Failed to deserialize CID: {:?}", err)))
    }

    /// Some Serde data formats interpret a byte stream as a sequence of bytes (e.g. `serde_json`).
    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: de::SeqAccess<'de>,
    {
        let mut bytes = Vec::new();
        while let Some(byte) = seq.next_element()? {
            bytes.push(byte);
        }
        Cid::try_from(bytes)
            .map_err(|err| de::Error::custom(format!("Failed to deserialize CID: {:?}", err)))
    }
}
