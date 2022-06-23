use crate::unsigned_varint::{to_varint, from_varint};

use sha3::{Digest, Sha3_256, Sha3_512};
use std::fmt;
//use std:error::Error;

#[derive(Debug, PartialEq, Clone)]
pub struct Multihash {
  code: u64,
  size: u64,
  digest: Vec<u8>,
}

impl Multihash {
  pub fn to_bytes(&self) -> Vec<u8> {
    let mut code = to_varint(self.code);
    code.extend(to_varint(self.size.into()));
    code.extend(self.digest.clone());
    code
  }

  pub fn from_bytes(bytes: &mut Vec<u8>) -> Result<Multihash, ()> {
    let code = from_varint(bytes)?;
    let size = from_varint(bytes)?;
    let digest = bytes.clone();
    if digest.len() as u64 > size {
      panic!("Bad size");
    }
    Ok(Multihash { code, size, digest })
  }

  pub fn sha3_256(bytes: &Vec<u8>) -> Multihash {
    let mut hasher = Sha3_256::new();
    hasher.update(bytes);
    let digest = hasher.finalize().to_vec();
    Multihash{ code: 0x16, size: 32, digest }
  }

  pub fn sha3_512(bytes: &Vec<u8>) -> Multihash {
    let mut hasher = Sha3_512::new();
    hasher.update(bytes);
    let digest = hasher.finalize().to_vec();
    Multihash{ code: 0x14, size: 64, digest }
  }
  
}

impl fmt::Display for Multihash {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}, {}, {:?}", self.code, self.size, self.digest)
  }
}

#[cfg(test)]
mod tests {
  use crate::multihash::Multihash;

  #[ignore]
  #[test]
  fn multihash_bytes_roundrip() {
    let data = vec![1];
    let result = Multihash::sha3_256(&data);
    assert_eq!(result, Multihash::from_bytes(&mut result.to_bytes()).unwrap());
  }
}
