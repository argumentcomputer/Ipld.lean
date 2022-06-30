use crate::ipld::Ipld;
use crate::cid::{Cid, CID_SERDE_PRIVATE_IDENTIFIER};

use serde::{ser, Serialize};

pub fn to_ipld<T>(value: T) -> Result<Ipld, ()>
where T: ser::Serialize {
  value.serialize(&Serializer)
}

impl ser::Serialize for Ipld {
  fn serialize<S>(&self, serializer:S) -> Result<S::Ok, S::Error>
  where S: ser::Serializer {
    match &self {
      Self::Null => serializer.serialize_none(),
      Self::Bool(value) => serializer.serialize_bool(*value),
      Self::Number(value) => serializer.serialize_u64(*value),
      Self::String(value) => serializer.serialize_str(value),
      Self::Bytes(value) => serializer.serialize_bytes(value),
      Self::Object(value) => serializer.collect_seq(value),
      Self::Link(value) => value.serialize(serializer),
    }
  }
}

struct Serializer;

pub struct StructSerializer {
  ser: Serializer,
  vec: Vec<Ipld>,
  variant_index: u32,
}

impl serde::Serializer for Serializer {
  type Ok = Ipld;
  type Error = ();

  type SerializeSeq = SerializeMap;
  type SerializeTuple = SerializeVec;
  type SerializeTupleStruct = StructSerializer;
  type SerializeTupleVariant = StructSerializer;
  type SerializeMap = SerializeVec;
  type SerializeStruct = SerializeVec;
  type SerializeStructVariant = SerializeTupleVariant;

  fn serialize_bool(self, value: bool) -> Result<Self::Ok, Self::Error> {
    self.serialize_bool(value)
  }
  fn serialize_i8(self, value: i8) -> Result<Self::Ok, Self::Error> {
    Err(Self::Error)
  }
  fn serialize_i16(self, value: i16) -> Result<Self::Ok, Self::Error> {
    Err(Self::Error)
  }
  fn serialize_i32(self, value: i32) -> Result<Self::Ok, Self::Error> {
    Err(Self::Error)
  }
  fn serialize_i64(self, value: i64) -> Result<Self::Ok, Self::Error> {
    Err(Self::Error)
  }
  fn serialize_u8(self, value: u8) -> Result<Self::Ok, Self::Error> {
    self.serialize_u64(u64::from(value))
  }
  fn serialize_u16(self, value: u16) -> Result<Self::Ok, Self::Error> {
    self.serialize_u64(u64::from(value))
  }
  fn serialize_u32(self, value: u32) -> Result<Self::Ok, Self::Error> {
    self.serialize_u64(u64::from(value))
  }
  fn serialize_u64(self, value: u64) -> Result<Self::Ok, Self::Error> {
    Ok(Self::Ok::Integer(value))
  }
  fn serialize_f32(self, value: f32) -> Result<Self::Ok, Self::Error> {
    Err(Self::Error)
  }
  fn serialize_f64(self, value: f64) -> Result<Self::Ok, Self::Error> {
    Err(Self::Error)
  }
  fn serialize_char(self, value: char) -> Result<Self::Ok, Self::Error> {
    self.serialize_str(&value.to_string())
  }
  fn serialize_str(self, value: &str) -> Result<Self::Ok, Self::Error> {
    Ok(Self::Ok::String(value.to_owned()))
  }
  fn serialize_bytes(self, value: &[u8]) -> Result<Self::Ok, Self::Error> {
    Ok(Self::Ok::Bytes(value.to_vec()))
  }
  
  fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
    Ok(Self::Ok::Null)
  }
  
  fn serialize_some<T: ?Sized>(
    self, 
    value: &T
  ) -> Result<Self::Ok, Self::Error>
  where T: Serialize {
    value.serialize(&self)
  }
  
  fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
    Ok(Self::Ok::Array(vec![]))
  }
  
  fn serialize_unit_struct(
    self, 
    name: &'static str
  ) -> Result<Self::Ok, Self::Error> {
    self.serialize_unit()
  }
  
  fn serialize_unit_variant(
    self, 
    name: &'static str, 
    variant_index: u32, 
    variant: &'static str
  ) -> Result<Self::Ok, Self::Error> {
    let idx = self.serialize_u64(variant_index)?;
    Ok(Self::Ok::Array(vec![idx]))
  }
  
  fn serialize_newtype_struct<T: ?Sized>(
    self, 
    name: &'static str, 
    value: &T
  ) -> Result<Self::Ok, Self::Error>
  where T: Serialize {
    let ipld = value.serialize(self);
    if name == CID_SERDE_PRIVATE_IDENTIFIER {
      if let Ok(Ipld::Bytes(bytes)) = ipld {
        let cid = Cid::try_from(bytes)
          .map_err(|err| ser::Error::custom(format!("Invalid CID: {}", err)))?;
        return Ok(Self::Ok::Link(cid));
      }
    }
    ipld
  }
  
  fn serialize_newtype_variant<T: ?Sized>(
    self, 
    name: &'static str, 
    variant_index: u32, 
    variant: &'static str, 
    value: &T
  ) -> Result<Self::Ok, Self::Error>
  where T: Serialize {
    let values =
      Vec::from([self.serialize_u32(variant_index)?, value.serialize(self)?]);
    Ok(Self::Ok::Array(values))
  }
  
  fn serialize_seq(
    self, 
    len: Option<usize>
  ) -> Result<Self::SerializeSeq, Self::Error> {
    Ok(SerializeVec { vec: Vec::with_capacity(len.unwrap_or(0)) })
  }
  
  fn serialize_tuple(
    self, 
    len: usize
  ) -> Result<Self::SerializeTuple, Self::Error> {
    self.serialize_seq(Some(len))
  }
  
  fn serialize_tuple_struct(
    self, 
    name: &'static str, 
    len: usize
  ) -> Result<Self::SerializeTupleStruct, Self::Error> {
    self.serialize_tuple(len)
  }
  
  fn serialize_tuple_variant(
    self, 
    name: &'static str, 
    variant_index: u32, 
    variant: &'static str, 
    len: usize
  ) -> Result<Self::SerializeTupleVariant, Self::Error> {
    Ok(SerializeTupleVariant {
      idx: variant_index,
      vec: Vec::with_capacity(len),
    })
  }
  
  fn serialize_map(
    self, 
    len: Option<usize>
  ) -> Result<Self::SerializeMap, Self::Error> {
    Ok(SerializeMap { vec: Vec::new(), next_key: None })
  }
  fn serialize_struct(
    self, 
    name: &'static str, 
    len: usize
  ) -> Result<Self::SerializeStruct, Self::Error> {
    Ok(StructSerializer { ser: &self, vec: Vec::new(), variant_index: 0 })
  }
  
  fn serialize_struct_variant(
    self, 
    name: &'static str, 
    variant_index: u32, 
    variant: &'static str, 
    len: usize
  ) -> Result<Self::SerializeStructVariant, Self::Error> {
    Ok(StructSerializer { ser: &self, vec: Vec::new(), variant_index })
  }
  
  fn is_human_readable(&self) -> bool { false }
  
}

pub struct SerializeVec {
  vec: Vec<Ipld>,
}

pub struct SerializeTupleVariant {
  idx: u32,
  vec: Vec<Ipld>,
}

pub struct SerializeMap {
  vec: Vec<Ipld>,
  next_key: Option<Ipld>,
}

impl ser::SerializeSeq for SerializeVec {
  type Error = Self::Error;
  type Ok = Ipld;

  fn serialize_element<T: ?Sized>(
    &mut self,
    value: &T,
  ) -> Result<(), Self::Error>
  where
    T: ser::Serialize,
  {
    self.vec.push(value.serialize(&Serializer)?);
    Ok(())
  }

  fn end(self) -> Result<Self::Ok, Self::Error> { Ok(Self::Ok::List(self.vec)) }
}

impl ser::SerializeTuple for SerializeVec {
  type Error = Self::Error;
  type Ok = Ipld;

  fn serialize_element<T: ?Sized>(
    &mut self,
    value: &T,
  ) -> Result<(), Self::Error>
  where
    T: ser::Serialize,
  {
    ser::SerializeSeq::serialize_element(self, value)
  }

  fn end(self) -> Result<Self::Ok, Self::Error> { ser::SerializeSeq::end(self) }
}

impl ser::SerializeTupleStruct for SerializeVec {
  type Error = Self::Error;
  type Ok = Ipld;

  fn serialize_field<T: ?Sized>(
    &mut self,
    value: &T,
  ) -> Result<(), Self::Error>
  where
    T: ser::Serialize,
  {
    ser::SerializeSeq::serialize_element(self, value)
  }

  fn end(self) -> Result<Self::Ok, Self::Error> { ser::SerializeSeq::end(self) }
}

impl ser::SerializeTupleVariant for SerializeTupleVariant {
  type Error = Self::Error;
  type Ok = Ipld;

  fn serialize_field<T: ?Sized>(
    &mut self,
    value: &T,
  ) -> Result<(), Self::Error>
  where
    T: ser::Serialize,
  {
    self.vec.push(value.serialize(&Serializer)?);
    Ok(())
  }

  fn end(self) -> Result<Self::Ok, Self::Error> {
    let mut vec = Vec::new();
    let mut args = self.vec.clone();
    vec.push(Ipld::Integer(self.idx.clone() as i128));
    vec.append(&mut args);
    Ok(Ipld::List(vec))
  }
}

impl ser::SerializeMap for SerializeMap {
  type Error = Self::Error;
  type Ok = Ipld;

  fn serialize_key<T: ?Sized>(&mut self, key: &T) -> Result<(), Self::Error>
  where T: ser::Serialize {
    match key.serialize(&Serializer)? {
      key => {
        self.next_key = Some(key);
        Ok(())
      }
    }
  }

  fn serialize_value<T: ?Sized>(
    &mut self,
    value: &T,
  ) -> Result<(), Self::Error>
  where
    T: ser::Serialize,
  {
    let key = self.next_key.take();
    // Panic because this indicates a bug in the program rather than an
    // expected failure.
    let key = key.expect("serialize_value called before serialize_key");
    self.vec.push(Ipld::List(vec![key, value.serialize(&Serializer)?]));
    Ok(())
  }

  fn end(self) -> Result<Self::Ok, Self::Error> { Ok(Self::Ok::List(self.vec)) }
}

impl StructSerializer {
  #[inline]
  fn serialize_field_inner<T>(
    &mut self,
    _: &'static str,
    value: &T,
  ) -> Result<(), ()>
  where
    T: ?Sized + ser::Serialize,
  {
    let val = value.serialize(self.ser)?;
    self.vec.push(val);
    Ok(())
  }

  #[inline]
  fn skip_field_inner(&mut self, _: &'static str) -> Result<(), ()> {
    Ok(())
  }

  #[inline]
  fn end_inner(self) -> Result<Vec<Ipld>, ()> { Ok(self.vec) }
}

impl ser::SerializeStruct for StructSerializer {
  type Error = Self::Error;
  type Ok = Ipld;

  #[inline]
  fn serialize_field<T: ?Sized>(
    &mut self,
    key: &'static str,
    value: &T,
  ) -> Result<(), Self::Error>
  where
    T: ser::Serialize,
  {
    self.serialize_field_inner(key, value)?;
    Ok(())
  }

  #[inline]
  fn skip_field(&mut self, key: &'static str) -> Result<(), ()> {
    self.skip_field_inner(key)
  }

  #[inline]
  fn end(self) -> Result<Self::Ok, ()> {
    let x = self.end_inner()?;
    Ok(Ipld::List(x))
  }
}

impl ser::SerializeStructVariant for StructSerializer {
  type Error = Self::Error;
  type Ok = Ipld;

  fn serialize_field<T: ?Sized>(
    &mut self,
    key: &'static str,
    value: &T,
  ) -> Result<(), Self::Error>
  where
    T: ser::Serialize,
  {
    self.serialize_field_inner(key, value)?;
    Ok(())
  }

  fn end(self) -> Result<Self::Ok, Self::Error> {
    let mut vec = Vec::new();
    vec.push(Ipld::Integer(self.variant_index.clone() as i128));
    let mut args = self.end_inner()?;
    vec.append(&mut args);
    Ok(Ipld::List(vec))
  }
}

#[cfg(test)]
mod tests {
  use crate::serde::to_ipld;
  use crate::ipld::Ipld;

  #[test]
  fn val_to_ipld() {
    let data = vec![];
    let result = Ipld::Array(vec![]);
    assert_eq!(
      to_ipld(data),
      result,
    );
  }
}
