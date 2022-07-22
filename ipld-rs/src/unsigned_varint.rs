use std::convert::TryFrom;

// TODO: rewrite with bitvec
// TODO: rewrite with iterators

pub fn to_varint(mut y: u64) -> Vec<u8> {
  let mut result: Vec<u8> = vec![];
  loop {
    let b: u8 = u8::try_from(y % 128).unwrap();
    y /= 128;
    if y == 0 {
      result.push(b);
      break;
    }
    else {
      result.push(b + 128);
    }
  }
  result
}

pub fn from_varint(bytes: &[u8]) -> Result<u64, ()> {
  if bytes.is_empty() {
    return Err(());
  }
  let mut result: u64 = 0;
  for (i, item) in bytes.iter().enumerate() {
    let b = (*item as u64 % 128) << (i * 7);
    if item / 128 == 0 {
      result += b as u64;
      break;
    }
    else {
      result += b as u64;
    }
  }
  Ok(result)
}

pub fn varint_read_u64<R: std::io::Read>(r: &mut R) -> Result<u64, String> {
  let mut b = [0u8; 8];
  for i in 0..b.len() {
    let n = r.read(&mut (b[i..i + 1])).unwrap();
    if n == 0 {
      return Err("Varint decode error".into());
    }
    else if b[i] & 0x80 == 0 {
      return Ok(from_varint(&b[..=i]).unwrap());
    }
  }
  Err("Varint overflow error".into())
}

#[cfg(test)]
mod tests {
  use crate::unsigned_varint::{
    from_varint,
    to_varint,
  };

  #[test]
  fn varint_roundtrip() {
    assert_eq!(from_varint(&vec![160, 141, 6]).unwrap(), 100000);
    assert_eq!(from_varint(&to_varint(50)).unwrap(), 50);
  }
}
