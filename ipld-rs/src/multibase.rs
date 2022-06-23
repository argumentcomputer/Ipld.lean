use crate::multibase_impl;


// To create a Multibase trait, each of the following functions would be
// required by any instance

#[derive(Debug, PartialEq, Clone)]
pub struct Multibase {
  base: 
  code: char,
  alpha: String,
  rfc4648: bool,
  pad: bool,
}

impl Multibase {
  // Is this function useful in Rust? 
  pub fn digit(&self, d: u64) -> char {
    
  }

  pub fn read(&self, c: char) -> Option<u64> {}

  // Return the first Unicode scalar value in the string
  fn zero(&self) -> char {
    self.alpha.chars().next()
  }

  // Return the number of scalar values in the string
  fn base(&self) -> u64 {
    self.alpha.chars().count()
  }

  fn log2_base(&self) -> u64 {
    (self.base() as f32).log2() as u64
  }

  fn group(&self) -> u64 {
    let x = self.log2_base();
    if x % 8 == 0 {
      x
    }
    else if x % 4 == 0{
      x * 2
    }
    else if x % 2 == 0{
      x * 4
    }
    else {
      x * 8
    }
  }

}
