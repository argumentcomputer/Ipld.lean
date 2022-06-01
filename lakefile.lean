import Lake

open Lake DSL

package Ipld {
  defaultFacet := PackageFacet.oleans
  dependencies := #[{
    name := `LSpec
    src := Source.git "https://github.com/yatima-inc/LSpec.git" "a11e09c75ca02d9e43ab280417b67e7a30fccee5"
  }]
}
