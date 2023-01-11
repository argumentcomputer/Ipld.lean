import Lake

open Lake DSL

package Ipld

@[default_target]
lean_lib Ipld

require LSpec from git
  "https://github.com/yatima-inc/LSpec.git" @ "88f7d23e56a061d32c7173cea5befa4b2c248b41"

require YatimaStdLib from git
  "https://github.com/yatima-inc/YatimaStdLib.lean" @ "52d70775731234886aebd2852199ab39f395d6ac"

lean_exe Tests.DagCbor
lean_exe Tests.Multibase
lean_exe Tests.Multihash
lean_exe Tests.UnsignedVarInt
