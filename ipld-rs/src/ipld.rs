//! Ipld representation.

use std::collections::BTreeMap;
use crate::cid::Cid;

use std::fmt;

/// Ipld
#[derive(Clone, PartialEq, Debug)]
pub enum Ipld {
  /// Represents the absence of a value or the value undefined.
  Null,
  /// Represents a boolean value.
  Bool(bool),
  /// Represents an integer.
  Integer(i64),
  /// Represents an UTF-8 string.
  String(String),
  /// Represents a sequence of bytes.
  Bytes(Vec<u8>),
  /// Represents a list.
  Array(Vec<Ipld>),
  /// Represents a map of strings.
  Map(BTreeMap<String, Ipld>),
  /// Represents a link.
  Link(Cid),
}

impl Ipld {
  pub fn to_object(obs: Vec<(String, Ipld)>) -> Ipld {
    let mut res = BTreeMap::new();
    for (k, v) in obs {
      res.insert(k, v);
    }
    Ipld::Map(res)
  } 

  pub fn to_string(&self) -> String {
    match self {
      Ipld::Null => "Ipld.null".to_string(),
      Ipld::Bool(b) => format!("(Ipld.bool {b})"),
      Ipld::Integer(n) => format!("(Ipld.number {n})"),
      Ipld::String(s) => format!("(Ipld.string {s})"),
      Ipld::Bytes(b) => format!("(Ipld.bytes {:?})", b),
      Ipld::Array(obs) => {
        let obs: Vec<String> = obs.into_iter().map(|x| x.to_string()).collect();
        format!("(Ipld.array {:?})", obs)
      },
      Ipld::Map(obs) => {
        let obs = obs.into_iter()
                     .map(|(s, ob)| (s.clone(), ob.to_string()))
                     .collect::<Vec<(String, String)>>();
        format!("(Ipld.object {:?})", obs)
      },
      Ipld::Link(cid) => format!("(Ipld.link {cid})"),
    }
  }
}

impl fmt::Display for Ipld {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.to_string())
  }
}
