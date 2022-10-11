import Lake

open Lake DSL

package Ipld

lean_lib Ipld

require LSpec from git
  "https://github.com/yatima-inc/LSpec.git" @ "a8dc2f38fc38f16efcc877ca8a4c7b37d3965db0"

require YatimaStdLib from git
  "https://github.com/yatima-inc/YatimaStdLib.lean" @ "23b4926a7ab6b86f2051dc60e77646d977ace8bc"

lean_exe Tests.DagCbor
lean_exe Tests.Multibase
lean_exe Tests.Multihash
lean_exe Tests.UnsignedVarInt
