mod de;
mod ser;

mod error {
  #[derive(Clone, Debug)]
  pub struct SerdeError(String);

  impl core::fmt::Display for SerdeError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
      write!(f, "Serde error: {}", self.0)
    }
  }

  impl serde::de::Error for SerdeError {
    fn custom<T: core::fmt::Display>(msg: T) -> Self {
      Self(msg.to_string())
    }
  }
  impl serde::ser::Error for SerdeError {
    fn custom<T: core::fmt::Display>(msg: T) -> Self {
      Self(msg.to_string())
    }
  }
  
  impl serde::ser::StdError for SerdeError {}
}

//pub use error::SerdeError;

#[cfg(test)]
mod tests {
  use crate::serde::{
    ser::to_ipld,
    de::from_ipld,
  };
  use crate::ipld::Ipld;
  use serde::{Serialize, Deserialize};

  #[test]
  fn val_to_ipld() {
    let data: Vec<Ipld> = vec![];
    let result = Ipld::Array(vec![]);
    assert_eq!(
      to_ipld(data).unwrap(),
      result,
    );
  }

  #[derive(Serialize, Deserialize, Debug)]
  struct Point {
    x: u32,
    y: u32,
  }

  #[test]
  fn roundtrip() {
    let point = Point { x: 1, y: 2 };
    //let ser_result = Ipld::Array(vec![Ipld::Number(1), Ipld::Number(2)]);
    assert_eq!(from_ipld::<T>(to_ipld(point).unwrap()).unwrap(), point);
  }
}
