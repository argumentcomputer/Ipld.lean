use std::convert::TryFrom;

// TODO: rewrite with bitvec
// TODO: rewrite with iterators

pub fn to_varint(mut y: u64) -> Vec<u8> {
    let mut result: Vec<u8> = vec![];
    loop {
        let b: u8 = u8::try_from(y % 128).unwrap();
        y = y / 128;
        if y == 0 {
            result.push(b);
            break;
        } else {
            result.push(b + 128);
        }
    }
    result
}

pub fn from_varint(bytes: &[u8]) -> Result<u64, ()> {
    if bytes.len() == 0 {
        return Err(());
    }
    let mut result: u64 = 0;
    for i in 0..bytes.len() {
        let b = (bytes[i] as u64 % 128) << (i * 7);
        if bytes[i] / 128 == 0 {
            result += b as u64;
            break;
        } else {
            result += b as u64;
        }
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use crate::unsigned_varint::{from_varint, to_varint};

    #[test]
    fn varint_test() {
      assert_eq!(from_varint(&vec![160, 141, 6]).unwrap(), 100000);
      assert_eq!(from_varint(&to_varint(50)).unwrap(), 50);
    }
}
