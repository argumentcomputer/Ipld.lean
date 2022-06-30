use crate::multihash::Multihash;
use crate::unsigned_varint::{from_varint, to_varint};

use std::fmt;

pub const CID_SERDE_PRIVATE_IDENTIFIER: &str = "$__private__serde__identifier__for__cid";

#[derive(Debug, PartialEq, Clone)]
pub struct Cid {
    version: u64,
    codec: u64,
    hash: Multihash,
}

impl Cid {
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = vec![];
        bytes.extend(to_varint(self.version));
        bytes.extend(to_varint(self.codec));
        bytes.extend(self.hash.to_bytes());
        bytes
    }

    pub fn from_bytes(bytes: &mut Vec<u8>) -> Result<Cid, ()> {
        let version = from_varint(bytes)?;
        let codec = from_varint(bytes)?;
        let hash = Multihash::from_bytes(bytes)?;
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
