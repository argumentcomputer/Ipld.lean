import Ipld.Multihash

structure Cid where
  version : Nat
  codec: Nat
  hash: Multihash