import Ipld.Cid
import Std.Data.RBTree

open Std (RBNode)

inductive Ipld where
| null
| bool (b : Bool)
| number (n : UInt64)
| string (s : String)
| byte (b : ByteArray)
| array (elems : Array Ipld)
| object (kvPairs : RBNode String (fun _ => Ipld))
| link (cid: Cid)
deriving BEq, Inhabited

namespace Ipld

def mkObject (o : List (String × Ipld)) : Ipld :=
  object <| Id.run <| do
    let mut kvPairs := RBNode.leaf
    for (k, v) in o do
      kvPairs := kvPairs.insert compare k v
    kvPairs

end Ipld
