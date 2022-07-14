//use crate::multibase_impl;

#[derive(Debug, PartialEq, Clone)]
pub struct Multibase {
  //base: String,
  code: char,
  alpha: String,
  rfc4648: bool,
  pad: bool,
}

impl Multibase {
  pub fn digit(&self, i: usize) -> char {
    if i < self.alpha.chars().count() {
      self.alpha.chars().nth(i).unwrap()
    }
    else {
      self.zero()
    }
  }
  
  //pub fn read(&self, c: char) -> Option<u64> {}
  
  // Return the first scalar value in the string
  fn zero(&self) -> char {
    self.alpha.chars().next().unwrap()
  }
  
  // Return the number of scalar values in the string
  fn base(&self) -> u64 {
    self.alpha.chars().count() as u64
  }
  
  fn log2_base(&self) -> u64 {
    (self.base() as f32).log2() as u64
  }
  
  fn group(&self) -> u64 {
    let x = self.log2_base();
    if x % 8 == 0 {
      x
    } else if x % 4 == 0 {
      x * 2
    } else if x % 2 == 0 {
      x * 4
    } else {
      x * 8
    }
  }
  
  fn encode(&self, mut x: Vec<u8>) -> String {
    //let log = self.log2_base();
    //let lzs = leading_zero_bits(x);
    //let rfc = self.rfc4648;
    //let zeros = match rfc {
      //true => vec![self.zero, lzs / log],
      //_ => vec![self.zero, lzs / 8],
    //};
    //let grp = self.group() / 8;
    //let byte_pad = (grp - x.len() % grp) % grp;
    //if rfc {
      //x.extend(vec![0; byte_pad]);
    //}
    //let n = read_be_u128(&x);
    //let mut encode_str = 5;
    //let char_pad = (byte_pad * 8) / log;
    //if rfc {
      //encode_str = drop_right(encode_str, char_pad);
      //if self.pad {
	//encode_str = pad_right(encode_str, char_pad);
      //}
    //}
    todo!()
  }
  
  fn decode(&self, input: &str) -> Result<(Multibase, Vec<u8>), String> {
    //let code = input.chars().next().ok_or(Error::InvalidBaseString)?;
    //let base = self.from_code(code)?;
    //let decoded = base.decode(&input[code.len_utf8()..])?;
    todo!()
  }
}

// Convert unsized slice into a 16-element array, then read into BE u128 
fn read_be_u128(input: &[u8]) -> Result<u128, String> {
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

fn leading_zero_bits(x: Vec<u8>) -> u64 {
  let mut zeros = 0;
  for byte in x {
    zeros += byte.leading_zeros();
    if byte != 0 {
      break;
    }
  }
  zeros.into()
}

pub fn encode_bytes(input: Vec<u8>) -> String {
  todo!()
}

pub fn decode_bytes(x: String) -> Result<Vec<u8>, String> {
  //Multibase::decode(x).to_string()
  todo!()
}
