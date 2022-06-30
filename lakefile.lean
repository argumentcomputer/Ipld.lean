import Lake

open Lake DSL

package Ipld {
  defaultFacet := PackageFacet.oleans
  dependencies := #[{
    name := `LSpec
    src := Source.git "https://github.com/yatima-inc/LSpec.git" "56da3b774818df05f44d3fc7621a6888b716ee4a"
  }, {
    name := "YatimaStdLib",
    src := Source.git "https://github.com/yatima-inc/YatimaStdLib.lean" "10110f7311a83c76a0f83f709b8ceea3abcb76e9"
  }]
}
