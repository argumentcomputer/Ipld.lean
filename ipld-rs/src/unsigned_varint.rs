use std::convert::TryFrom;

pub fn to_varint(mut y: u64) -> Vec<u8> {
  let mut result: Vec<u8> = vec![];
  for _ in (0..(y+1)).rev() {
    let b: u8 = u8::try_from(y % 128).unwrap();
    y = y / 128;
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

// TODO
pub fn from_varint(_bytes: &Vec<u8>) -> Result<u64, ()> {
  Ok(0)
}
