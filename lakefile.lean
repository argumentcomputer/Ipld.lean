import Lake

open Lake DSL

package Ipld

lean_lib Ipld

require LSpec from git
  "https://github.com/yatima-inc/LSpec.git" @ "7e2d41644519e8c437fbe7461544eaa855738f73"

require YatimaStdLib from git
  "https://github.com/yatima-inc/YatimaStdLib.lean" @ "9c362443e0d89eb96683b52a1caaf762049697c4"

lean_exe Tests.DagCbor
lean_exe Tests.Multibase
lean_exe Tests.Multihash
lean_exe Tests.UnsignedVarInt
