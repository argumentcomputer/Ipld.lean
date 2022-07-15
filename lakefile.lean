import Lake

open Lake DSL

package Ipld

lean_lib Ipld

require LSpec from git
  "https://github.com/yatima-inc/LSpec.git" @ "3b759f6e7798fdb6b17ae83ea060cd34e89b7e91"

require YatimaStdLib from git
  "https://github.com/yatima-inc/YatimaStdLib.lean" @ "80b290a322267aee7dbca96b2547fa24de64236a"

lean_exe Tests.DagCbor
lean_exe Tests.Multibase
lean_exe Tests.Multihash
lean_exe Tests.UnsignedVarInt
