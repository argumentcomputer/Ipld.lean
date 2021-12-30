import Ipld.Cid
import Ipld.Multihash
import Ipld.Utils
import Std.Data.RBTree

open Std (RBNode)

def toList (map : RBNode α (fun _ => β)) : List (α × β) :=
  map.revFold (fun as a b => (a,b)::as) []

instance [BEq α] [BEq β] : BEq (RBNode α fun _ => β) where
  beq a b := toList a == toList b
  
instance : BEq Cid where
  beq a b := a.version == b.version && a.codec == b.codec && a.hash == b.hash

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
