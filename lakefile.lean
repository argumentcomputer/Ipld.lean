import Lake

open Lake DSL

package Ipld

lean_lib Ipld

require LSpec from git
  "https://github.com/yatima-inc/LSpec.git" @ "a8dc2f38fc38f16efcc877ca8a4c7b37d3965db0"

require YatimaStdLib from git
  "https://github.com/yatima-inc/YatimaStdLib.lean" @ "cbf5cd7c098c4369d93b9b8399a323bf0a28c107"

lean_exe Tests.DagCbor
lean_exe Tests.Multibase
lean_exe Tests.Multihash
lean_exe Tests.UnsignedVarInt
