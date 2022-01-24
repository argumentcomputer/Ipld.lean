import Ipld.Multibase
import Ipld.Keccak

open Multibase
open Keccak

def main : IO Unit := do
  let x := Nat.toByteArrayBE 3
  println! "keccak256(3 → \"{encodeBytes Base64 x}\")={encodeBytes Base64 (Keccak.keccak256 x)}"

#eval (keccak256 "hello".toUTF8)
#eval (keccak256 "abcdefghijklmnopqrstuvwxyz234567".toUTF8)
