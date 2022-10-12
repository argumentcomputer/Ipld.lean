import Lake

open Lake DSL

package Ipld

lean_lib Ipld

require LSpec from git
  "https://github.com/yatima-inc/LSpec.git" @ "a8dc2f38fc38f16efcc877ca8a4c7b37d3965db0"

require YatimaStdLib from git
  "https://github.com/yatima-inc/YatimaStdLib.lean" @ "9aedefe96478a6033c1ffa030a800f4d3e0c7681"

lean_exe Tests.DagCbor
lean_exe Tests.Multibase
lean_exe Tests.Multihash
lean_exe Tests.UnsignedVarInt
