import Lake

open Lake DSL

package Ipld

lean_lib Ipld

require LSpec from git
  "https://github.com/yatima-inc/LSpec.git" @ "59de5823f0f055f06cacd7c066fff6c3e14b13a2"

require YatimaStdLib from git
  "https://github.com/yatima-inc/YatimaStdLib.lean" @ "a2bbc9a48db7efd5d761a5b27f2cc6c1863b9622"

lean_exe Tests.DagCbor
lean_exe Tests.Multibase
lean_exe Tests.Multihash
lean_exe Tests.UnsignedVarInt
