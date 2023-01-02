import Lake

open Lake DSL

package Ipld

@[default_target]
lean_lib Ipld

require LSpec from git
  "https://github.com/yatima-inc/LSpec.git" @ "129fd4ba76d5cb9abf271dc29208a28f45fd981e"

require YatimaStdLib from git
  "https://github.com/yatima-inc/YatimaStdLib.lean" @ "e3078e5496b3c5e635792081ea1a483b33ed536b"

lean_exe Tests.DagCbor
lean_exe Tests.Multibase
lean_exe Tests.Multihash
lean_exe Tests.UnsignedVarInt
