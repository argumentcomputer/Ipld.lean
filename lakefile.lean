import Lake

open Lake DSL

package Ipld

lean_lib Ipld

require LSpec from git
  "https://github.com/yatima-inc/LSpec.git" @ "70e792016f8e2682fbcf65624708c85ceaf8db63"

require YatimaStdLib from git
  "https://github.com/yatima-inc/YatimaStdLib.lean" @ "35aecd8951778f45a47d12376635c26a815dcb25"

lean_exe Tests.DagCbor
lean_exe Tests.Multibase
lean_exe Tests.Multihash
lean_exe Tests.UnsignedVarInt
