import Lake

open Lake DSL

package Ipld

@[default_target]
lean_lib Ipld

require LSpec from git
  "https://github.com/yatima-inc/LSpec.git" @ "02e423d02d2ba1b76bed3cf6459a5c2d7a13afb8"

require YatimaStdLib from git
  "https://github.com/yatima-inc/YatimaStdLib.lean" @ "2b914196a8c67838e63c1c1e44eaf339b8a50eb7"

lean_exe Tests.DagCbor
lean_exe Tests.Multibase
lean_exe Tests.Multihash
lean_exe Tests.UnsignedVarInt
