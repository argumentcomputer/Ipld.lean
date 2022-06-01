import Lake

open Lake DSL

package Ipld {
  defaultFacet := PackageFacet.oleans
  dependencies := #[{
    name := `LSpec
    src := Source.git "https://github.com/yatima-inc/LSpec.git" "f6e2c5f5398e6950631cc193266695ed0cfd47ff"
  }]
}
