import Ipld.UnsignedVarint
import Ipld.Utils
import Ipld.Multihash
import Ipld.Multibase
import Ipld.Cid
import Ipld.Keccak
import Ipld.Ipld
import Blake3

def main : IO Unit := do
  println! "Hello, world! {Blake3.hash "hello"}"
  let x := Nat.toByteArrayBE 3
  println! "test {Blake3.hash x}"
