import Lake

open Lake DSL

package Ipld {
  defaultFacet := PackageFacet.oleans
  dependencies := #[{
    name := `LSpec
    src := Source.git "https://github.com/yatima-inc/LSpec.git" "f1497c32efdaab4c707b86caa688ab5efeb10110"
  }, {
    name := `YatimaStdLib 
    src := Source.git "https://github.com/yatima-inc/YatimaStdLib.lean.git" "10110f7311a83c76a0f83f709b8ceea3abcb76e9" 
  }]
}
