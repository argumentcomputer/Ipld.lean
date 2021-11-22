abbrev Data := UInt64
abbrev Name := String
abbrev MVarId := String
abbrev Level := UInt64

inductive Literal where
  | natVal (val : Nat)
  | strVal (val : String)
  deriving Inhabited, BEq, Repr

inductive DataValue where
  | ofString (v : String)
  | ofBool   (v : Bool)
  | ofName   (v : Name)
  | ofNat    (v : Nat)
  | ofInt    (v : Int)
  deriving Inhabited, BEq

structure KVMap where
  entries : List (Name × DataValue) := []
  deriving Inhabited

abbrev MData := KVMap

structure FVarId where
  name : Name
  deriving Inhabited, BEq, Hashable

inductive Expr where
  | bvar    : Nat → Data → Expr                       -- bound variables
  | fvar    : FVarId → Data → Expr                    -- free variables
  | mvar    : MVarId → Data → Expr                    -- meta variables
  | sort    : Level → Data → Expr                     -- Sort
  | const   : Name → List Level → Data → Expr         -- constants
  | app     : Expr → Expr → Data → Expr               -- application
  | lam     : Name → Expr → Expr → Data → Expr        -- lambda abstraction
  | forallE : Name → Expr → Expr → Data → Expr        -- (dependent) arrow
  | letE    : Name → Expr → Expr → Expr → Data → Expr -- let expressions
  | lit     : Literal → Data → Expr                   -- literals
  | mdata   : MData → Expr → Data → Expr              -- metadata
  | proj    : Name → Nat → Expr → Data → Expr         -- projection
  deriving Inhabited
