import Lake

open Lake DSL

package Ipld {
  defaultFacet := PackageFacet.oleans
  dependencies := #[{
    name := `LSpec
    src := Source.git "https://github.com/yatima-inc/LSpec.git" "04df8a2b03c7d1e2ff24eb14eed44c0a82218863"
  }, {
    name := "YatimaStdLib",
    src := Source.git "https://github.com/yatima-inc/YatimaStdLib.lean" "10110f7311a83c76a0f83f709b8ceea3abcb76e9"
  }]
}
