import Ipld.UnsignedVarInt
import Ipld.Utils
import Ipld.Multihash
import Ipld.Multibase
import Ipld.Cid
import Ipld.Keccak
import Ipld.Ipld
import Ipld.DagCbor


def cid : Option Cid :=
  @Cid.fromString Multibase.Base32 Multibase.base32_multibase_inst "b3c2sid3egjq36mrlrme5xrv7xmvx7ekiuntuue2ligfwas54av6a"

#eval cid
  
--ipld: (Ipld.array #[(Ipld.number 3235774465), (Ipld.number 1), (
--Ipld.link
-- b3c2sid3egjq36mrlrme5xrv7xmvx7ekiuntuue2ligfwas54av6a)])
--cbor: [131, 26, 192, 222, 0, 1, 1, 216, 42, 88, 33, 0, 216, 181, 36, 15, 100, 50, 97, 191, 50, 43, 139, 9, 219, 198, 191, 187, 43, 127, 145, 72, 163, 103, 74, 19, 75, 65, 139, 96, 75, 188, 5, 124]
--multihash: mFiD1vaFQMZMwBvCjIT4IMmUgV69kjhmvmVzLeNBADDUG8w
--multihash digest: [245, 189, 161, 80, 49, 147, 48, 6, 240, 163, 33, 62, 8, 50, 101, 32, 87, 175, 100, 142, 25, 175, 153, 92, 203, 120, 208, 64, 12, 53, 6, 243] 
