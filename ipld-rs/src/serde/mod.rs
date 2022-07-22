mod de;
mod ser;

// TODO: move to ../error.rs
mod error {
  #[derive(Clone, Debug)]
  pub struct SerdeError(String);

  impl core::fmt::Display for SerdeError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
      write!(f, "Serde error: {}", self.0)
    }
  }

  impl serde::de::Error for SerdeError {
    fn custom<T: core::fmt::Display>(msg: T) -> Self { Self(msg.to_string()) }
  }
  impl serde::ser::Error for SerdeError {
    fn custom<T: core::fmt::Display>(msg: T) -> Self { Self(msg.to_string()) }
  }

  impl serde::ser::StdError for SerdeError {}
}

// pub use error::SerdeError;

#[cfg(test)]
mod tests {
  use crate::{
    ipld::Ipld,
    serde::{
      de::from_ipld,
      ser::to_ipld,
    },
  };
  use serde::{
    de::DeserializeOwned,
    Deserialize,
    Serialize,
  };

  /// Utility for testing (de)serialization of [`Ipld`].
  ///
  /// Checks if `data` and `ipld` match if they are encoded into each other.
  fn assert_roundtrip<T>(data: &T, ipld: &Ipld)
  where T: Serialize + DeserializeOwned + PartialEq + std::fmt::Debug {
    let encoded: Ipld = to_ipld(&data).unwrap();
    assert_eq!(&encoded, ipld);
    let decoded: T = from_ipld(ipld.clone()).unwrap();
    assert_eq!(&decoded, data);
  }

  #[derive(Serialize, Deserialize, PartialEq, Debug)]
  struct Point {
    x: u32,
    y: u32,
  }

  #[test]
  fn ser_de_ipld() {
    let point = Point { x: 1, y: 2 };
    let expected = Ipld::Array(vec![Ipld::Number(1), Ipld::Number(2)]);
    assert_roundtrip(&point, &expected);
  }
}
