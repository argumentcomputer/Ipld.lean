import Lake

open Lake DSL

package Ipld

lean_lib Ipld

require LSpec from git
  "https://github.com/yatima-inc/LSpec.git" @ "ec5e874585b8122451fa4fe4428d1334a3d76075"

require YatimaStdLib from git
  "https://github.com/yatima-inc/YatimaStdLib.lean" @ "a2bbc9a48db7efd5d761a5b27f2cc6c1863b9622"

lean_exe Tests.DagCbor
lean_exe Tests.Multibase
lean_exe Tests.Multihash
lean_exe Tests.UnsignedVarInt
