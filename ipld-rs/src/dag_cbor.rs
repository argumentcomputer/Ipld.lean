use std::collections::BTreeMap;

use crate::cid::Cid;
use crate::ipld::Ipld;

pub fn serialize(ipld: &Ipld) -> Vec<u8> {
    match ipld {
        Ipld::Null => ser_null(),
        Ipld::Bool(b) => ser_bool(b),
        Ipld::Number(i) => ser_u64(0, *i),
        Ipld::String(s) => ser_string(&s),
        Ipld::Bytes(b) => ser_bytes(&b),
        Ipld::Array(a) => ser_array(&a),
        Ipld::Object(o) => ser_object(&o),
        Ipld::Link(c) => ser_link(&c),
    }
}

fn ser_null() -> Vec<u8> {
    vec![0xf6]
}

fn ser_bool(b: &bool) -> Vec<u8> {
    match b {
        true => vec![0xf5],
        false => vec![0xf4],
    }
}

fn ser_u8(major: u8, n: u8) -> Vec<u8> {
    if n <= 0x17 {
        vec![(major << 5) | n]
    } else {
        vec![(major << 5) | 24, n]
    }
}

fn ser_u16(major: u8, n: u16) -> Vec<u8> {
    if n <= 255 {
        ser_u8(major, u8::try_from(n).unwrap())
    } else {
        let mut buf = vec![(major << 5) | 25];
        buf.extend(n.to_be_bytes());
        buf
    }
}

fn ser_u32(major: u8, n: u32) -> Vec<u8> {
    if n <= 65535 {
        ser_u16(major, u16::try_from(n).unwrap())
    } else {
        let mut buf = vec![(major << 5) | 26];
        buf.extend(n.to_be_bytes());
        buf
    }
}

fn ser_u64(major: u8, n: u64) -> Vec<u8> {
    if n <= 4294967295 {
        ser_u32(major, u32::try_from(n).unwrap())
    } else {
        let mut buf = vec![(major << 5) | 27];
        buf.extend(n.to_be_bytes());
        buf
    }
}

fn ser_string(s: &String) -> Vec<u8> {
    let str_bytes = s.as_bytes();
    let mut result = ser_u64(3, str_bytes.len() as u64);
    result.extend(str_bytes);
    result
}

fn ser_bytes(b: &Vec<u8>) -> Vec<u8> {
    let mut result = ser_u64(2, (b.len() + 1) as u64);
    result.extend(b);
    result
}

fn ser_link(l: &Cid) -> Vec<u8> {
    let buf = l.to_bytes();
    let mut result = ser_u64(6, 42);
    result.extend(ser_u64(2, (buf.len() + 1) as u64));
    result.push(0);
    result.extend(buf);
    result
}

fn ser_array(a: &Vec<Ipld>) -> Vec<u8> {
    let mut result = ser_u64(4, a.len() as u64);
    for ipld in a {
        result.extend(serialize(ipld));
    }
    result
}

fn ser_object(m: &BTreeMap<String, Ipld>) -> Vec<u8> {
    let mut result = ser_u64(5, m.len() as u64);
    for (key, val) in m.iter() {
        result.extend(ser_string(key));
        result.extend(serialize(val));
    }
    result
}

//pub struct Deserializer;
//
//pub fn deserialize(bytes: Vec<u8>) {
//  let mut deserializer = Deserializer::new();
//  let major = read_u8();
//  match major {
//    0x18 => Ipld::Integer { read_u8(&mut deserializer) as u64 },
//    0x19 => Ipld::Integer 
//
//  }
//}
//
//fn read_u8(de: &mut Deserializer) {
//
//}
//
//fn read_u16() {
//
//}
//
//fn read_u32() {
//
//}
//
//fn read_u64() {
//
//}
  
