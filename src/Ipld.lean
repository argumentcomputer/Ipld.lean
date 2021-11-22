import Ipld.UnsignedVarint
import Ipld.Multihash
import Ipld.Multibase
import Ipld.Cid
import Ipld.Ipld
import Ipld.LeanAST
import Blake3

def main : IO Unit :=
  println! "Hello, world! {Blake3.hash "hello".toByteArray}"
