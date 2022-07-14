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
