import Lake

open Lake DSL

package Ipld

lean_lib Ipld

require LSpec from git
  "https://github.com/yatima-inc/LSpec.git" @ "95f4f5b865a5ac8044fc3c9c2c83a7220f806d4c"

require YatimaStdLib from git
  "https://github.com/yatima-inc/YatimaStdLib.lean" @ "80b290a322267aee7dbca96b2547fa24de64236a"

lean_exe Tests.DagCbor
lean_exe Tests.Multibase
lean_exe Tests.Multihash
lean_exe Tests.UnsignedVarInt
