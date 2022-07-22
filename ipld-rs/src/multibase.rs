// use crate::multibase_impl;

#[derive(Debug, PartialEq, Clone)]
pub struct Multibase {
  code: char,
  alpha: String,
  rfc4648: bool,
  pad: bool,
}

impl Multibase {
  // Returns the first scalar value in the alpha string
  fn zero(&self) -> char { self.alpha.chars().next().unwrap() }

  // Returns the number of scalar values in the alpha string
  fn base(&self) -> u64 { self.alpha.chars().count() as u64 }

  // Returns the log2 of the base
  fn log2_base(&self) -> u64 { (self.base() as f32).log2().trunc() as u64 }

  // Returns the RFC4648 base's group size in bits
  fn group(&self) -> u64 {
    let x = self.log2_base();
    if x % 8 == 0 {
      x
    }
    else if x % 4 == 0 {
      x * 2
    }
    else if x % 2 == 0 {
      x * 4
    }
    else {
      x * 8
    }
  }

  // Returns the character at the given index of the alpha string
  fn digit(&self, idx: usize) -> char {
    if idx < self.alpha.chars().count() {
      self.alpha.chars().nth(idx).unwrap()
    }
    else {
      self.zero()
    }
  }

  // Returns the byte index of the given character in the alpha string
  // Will not be equivalent to the character index for non-ASCII strings
  fn read(&self, c: char) -> Option<u64> {
    self.alpha.find(c).map(|idx| idx as u64)
  }

  // Checks if a char is contained in the alpha string
  fn valid_digit(&self, c: char) -> bool { self.read(c) != None }

  // Checks if all characters in a given string are contained in the alpha
  // string
  fn validate(&self, input: &str) -> bool {
    for c in input.chars() {
      if !self.valid_digit(c) {
        return false;
      }
    }
    true
  }

  // Appends a given number of '=' characters to a string
  fn pad_right(&self, input: &mut String, num_pad: usize) {
    let pad: String = "=".repeat(num_pad);
    input.push_str(&pad);
  }

  // Returns the number of leading zero bits in an array
  fn leading_zero_bits(&self, input: &[u8]) -> u64 {
    let mut zeros = 0;
    for byte in input {
      zeros += byte.leading_zeros();
      if *byte != 0 {
        break;
      }
    }
    zeros.into()
  }

  // Checks if first char of given string is the same as the Multibase code
  // If so, returns the rest of the string
  fn read_code(&self, input: &str) -> Option<String> {
    let mut iter = input.chars();
    match iter.next() {
      Some(c) => {
        if c == self.code {
          Some(iter.collect())
        }
        else {
          None
        }
      }
      _ => None,
    }
  }

  // Returns the number of characters equal to alpha[0] in a given string
  fn read_zeros(&self, input: &str) -> u64 {
    let zero = self.zero();
    let iter = input.chars();
    iter.fold(0, |acc, x| if x == zero { acc + 1 } else { acc })
  }

  // Converts a list of bytes into a base encoding
  pub fn encode(&self, mut input: Vec<u8>) -> String {
    let log = self.log2_base() as usize;
    let lzs = self.leading_zero_bits(&input) as usize;
    let rfc = self.rfc4648;
    let zeros: String = match rfc {
      true => self.zero().to_string().repeat(lzs / log),
      _ => self.zero().to_string().repeat(lzs / 8),
    };
    let grp = self.group() as usize / 8;
    let byte_pad = (grp - input.len() % grp) % grp;
    if rfc {
      input.extend(vec![0; byte_pad]);
    }
    // TODO: handle error gracefully
    // Converts a big-endian list of bytes into u128, then into numeric string
    // form Seems like a compression operation
    let n = read_be_u128(&input).unwrap();
    let mut encode_str = n.to_string();
    let char_pad = (byte_pad * 8) / log;
    if rfc {
      // TODO: Should it be char_pad - 1 (if char_pad > 0)?
      encode_str = encode_str.drain(..char_pad).collect();
      if self.pad {
        self.pad_right(&mut encode_str, char_pad);
      }
    }
    self.code.to_string() + &zeros + &encode_str
  }

  // Converts base-encoded bytes into base
  pub fn decode(&self, input: &str) -> Result<Vec<u8>, String> {
    // TODO: handle error gracefully
    let mut data: String = self.read_code(input).unwrap();
    let mut len = input.len();
    let mut zero_chars = self.read_zeros(&data) as usize;
    let log = self.log2_base() as usize;
    if self.rfc4648 {
      let grp = self.group() as usize;
      let chars = grp / log;
      let in_bits = len * log;
      let rem_bits = in_bits % grp;
      let pad = ((grp - rem_bits) / log) % chars;
      self.pad_right(&mut data, pad);
    }
    if data.is_empty() {
      Ok(vec![])
    }
    else {
      //println!("My data: {}", data);
      let data_num: u128 = data.parse().unwrap();
      // Converts to sized 16 byte BE array
      let out = data_num.to_be_bytes().to_vec();
      let mut pad_len = 0;
      // TODO: rewrite with iterators
      for i in out.iter().rev() {
        if *i == 0 {
          pad_len += 1;
        }
        else {
          break;
        }
      }
      if !self.pad {
        len += pad_len;
      }
      if self.rfc4648 {
        zero_chars = zero_chars * log / 8;
        if len * log > out.len() * 8 + zero_chars * log {
          zero_chars += 1;
        }
      }
      let mut pad_bytes = pad_len * log / 8;
      if (pad_len * log) % 8 == 0 {
        pad_bytes += 1;
      }
      let mut zeros: Vec<u8> = vec![0; zero_chars];
      zeros.extend(&out[..out.len() - pad_bytes]);
      Ok(zeros)
    }
  }
}

// Converts an unsized slice into a 16-element array, then reads it into
// big-endian u128
pub fn read_be_u128(input: &[u8]) -> Result<u128, String> {
  if input.len() > 16 {
    Err("Input too large".into())
  }
  else {
    let mut res = [0u8; 16];
    let mut idx = res.len() - 1;
    for i in input {
      res[idx] = *i;
      idx -= 1;
    }
    Ok(u128::from_be_bytes(res))
  }
}

//pub fn encode_bytes(input: Vec<u8>) -> String {
//  Multibase::encode(input)
//}
//pub fn decode_bytes(x: String) -> Result<Vec<u8>, String> {
//  Multibase::decode(x)
//}

#[cfg(test)]
mod tests {
  use crate::multibase::Multibase;
  
  #[ignore]
  #[test]
  fn multibase_roundtrip() {
    let data = b"Hello, world!".to_vec();
    let base2 = Multibase {
      code: '0',
      alpha: String::from("01"),
      rfc4648: true,
      pad: false,

    };
    let base32 = Multibase {
      code: 'b',
      alpha: String::from("abcdefghijklmnopqrstuvwxyz234567"),
      rfc4648: true,
      pad: false,
    };
    assert_eq!(data, base2.decode(&base2.encode(data.clone())).unwrap());
    assert_eq!(data, base32.decode(&base32.encode(data.clone())).unwrap());
  }
}
