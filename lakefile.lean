import Lake
open Lake DSL

package ipld {
  dependencies := #[{
    name := `blake3
    src := Source.git "https://github.com/yatima-inc/lean-blake3" "sb/lake" 
  }] 
  -- defaultFacet := PackageFacet.oleans
}
