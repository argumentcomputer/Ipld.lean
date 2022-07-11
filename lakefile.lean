import Lake

open Lake DSL

package Ipld

lean_lib Ipld

require LSpec from git
  "https://github.com/yatima-inc/LSpec.git" @ "95b36c3a13e32355a9222e1dad33e354c604798d"

require YatimaStdLib from git
  "https://github.com/yatima-inc/YatimaStdLib.lean" @ "35aecd8951778f45a47d12376635c26a815dcb25"

lean_exe Tests.DagCBor
lean_exe Tests.Multibase
lean_exe Tests.Multihash
lean_exe Tests.UnsignedVarInt
