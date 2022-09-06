use crate::unsigned_varint::{
  to_varint,
  varint_read_u64,
};

use sha3::{
  Digest,
  Sha3_256,
  Sha3_512,
};
use std::{
  fmt,
  io::Read,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Multihash {
  code: u64,
  size: u64,
  digest: Vec<u8>,
}

impl Multihash {
  pub fn to_bytes(&self) -> Vec<u8> {
    let mut code = to_varint(self.code);
    code.extend(to_varint(self.size));
    code.extend(self.digest.clone());
    code
  }

  pub fn from_bytes<R: Read>(r: &mut R) -> Result<Multihash, ()> {
    let code = varint_read_u64(r).unwrap();
    let size = varint_read_u64(r).unwrap();

    let mut digest = vec![0; size as usize];
    r.read_exact(&mut digest[..size as usize]).unwrap();
    Ok(Multihash { code, size, digest })
  }

  pub fn sha3_256(bytes: &Vec<u8>) -> Multihash {
    let mut hasher = Sha3_256::new();
    hasher.update(bytes);
    let digest = hasher.finalize().to_vec();
    Multihash { code: 0x16, size: 32, digest }
  }

  pub fn sha3_512(bytes: &Vec<u8>) -> Multihash {
    let mut hasher = Sha3_512::new();
    hasher.update(bytes);
    let digest = hasher.finalize().to_vec();
    Multihash { code: 0x14, size: 64, digest }
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

  #[test]
  fn multihash_bytes_roundtrip() {
    let data = vec![1];
    let result = Multihash::sha3_256(&data);
    assert_eq!(
      result,
      Multihash::from_bytes(&mut &result.to_bytes()[..]).unwrap()
    );
  }
}
