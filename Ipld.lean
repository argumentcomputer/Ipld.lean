import Ipld.UnsignedVarint
import Ipld.Multihash
import Ipld.Multibase
import Ipld.Cid
import Ipld.Ipld
import Ipld.LeanAST
import Blake3

def main : IO Unit := do
  println! "Hello, world! {Blake3.hash "hello".toByteArray}"
  let x := Nat.toByteArrayBE 3
  println! "test {Blake3.hash x}"
