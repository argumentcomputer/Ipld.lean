import Ipld.Ipld
import Ipld.UnsignedVarint

abbrev Data := UInt64
abbrev Name := String
abbrev FVarId := String
abbrev MVarId := String
abbrev Level := Nat

inductive Literal where
  | natVal (num : Nat)
  | strVal (str : String)
  deriving Inhabited, BEq, Repr

def litToIpld : Literal -> Ipld
  | Literal.natVal num => Ipld.byte $ Nat.toByteArrayBE num
  | Literal.strVal str => Ipld.string str

inductive DataValue where
  | ofString (v : String)
  | ofBool   (v : Bool)
  | ofName   (v : Name)
  | ofNat    (v : Nat)
  | ofInt    (v : Int)
  deriving Inhabited, BEq

def dataValueToIpld : DataValue -> Ipld
  | DataValue.ofString str =>
    let tag := Ipld.string "STR"
    let str := Ipld.string str
    Ipld.array #[tag, str]
  | DataValue.ofBool bool =>
    let tag := Ipld.string "BOOL"
    let bool := Ipld.bool bool
    Ipld.array #[tag, bool]
  | DataValue.ofName nam =>
    let tag := Ipld.string "NAM"
    let nam := Ipld.string nam
    Ipld.array #[tag, nam]
  | DataValue.ofNat num =>
    let tag := Ipld.string "NAT"
    let num := Ipld.byte $ Nat.toByteArrayBE num
    Ipld.array #[tag, num]
  | DataValue.ofInt (Int.ofNat num) =>
    let tag := Ipld.string "INT"
    let sign := Ipld.bool false
    let num := Ipld.byte $ Nat.toByteArrayBE num
    Ipld.array #[tag, sign, num]
  | DataValue.ofInt (Int.negSucc num) =>
    let tag := Ipld.string "INT"
    let sign := Ipld.bool true
    let num := Ipld.byte $ Nat.toByteArrayBE num
    Ipld.array #[tag, sign, num]

structure KVMap where
  entries : List (Name × DataValue) := []
  deriving Inhabited

abbrev MData := KVMap

def mDataToIpld (mdat : MData) : Ipld :=
  let arr := #[]
  let func := λ (name, val) =>
    Ipld.array #[Ipld.string name, dataValueToIpld val]
  let list := List.map func mdat.entries
  Ipld.array $ Array.appendList arr list

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

-- Temporary version of expr->ipld.
-- This will give a flat representation of expressions. In the future
-- the nodes will be referenced by CIDs instead
def exprToIpld : Expr -> Ipld
  | Expr.bvar    idx dat =>
    let tag := Ipld.string "BVA"
    let idx := Ipld.byte $ Nat.toByteArrayBE idx
    let dat := Ipld.number dat
    Ipld.array #[tag, idx, dat]
  | Expr.fvar    nam dat =>
    let tag := Ipld.string "FVA"
    let nam := Ipld.string nam
    let dat := Ipld.number dat
    Ipld.array #[tag, nam, dat]
  | Expr.mvar    nam dat =>
    let tag := Ipld.string "MVA"
    let nam := Ipld.string nam
    let dat := Ipld.number dat
    Ipld.array #[tag, nam, dat]
  | Expr.sort    lvl dat =>
    let tag := Ipld.string "SOR"
    let lvl := Ipld.byte $ Nat.toByteArrayBE lvl
    let dat := Ipld.number dat
    Ipld.array #[tag, lvl, dat]
  | Expr.const   nam lvs dat => Ipld.null
  | Expr.app     fnc arg dat =>
    let tag := Ipld.string "APP"
    let fnc := exprToIpld fnc
    let arg := exprToIpld arg
    let dat := Ipld.number dat
    Ipld.array #[tag, fnc, arg, dat]
  | Expr.lam     nam typ bod dat =>
    let tag := Ipld.string "LAM"
    let nam := Ipld.string nam
    let bod := exprToIpld bod
    let dat := Ipld.number dat
    Ipld.array #[tag, nam, bod, dat]
  | Expr.forallE nam dom cod dat =>
    let tag := Ipld.string "FOR"
    let nam := Ipld.string nam
    let dom := exprToIpld dom
    let cod := exprToIpld cod
    let dat := Ipld.number dat
    Ipld.array #[tag, nam, dom, cod, dat]
  | Expr.letE    nam typ val bod dat =>
    let tag := Ipld.string "LET"
    let nam := Ipld.string nam
    let typ := exprToIpld typ
    let val := exprToIpld val
    let bod := exprToIpld bod
    let dat := Ipld.number dat
   Ipld.array #[tag, nam, typ, val, bod, dat]
  | Expr.lit     lit dat =>
    let tag := Ipld.string "LIT"
    let lit := litToIpld lit
    let dat := Ipld.number dat
    Ipld.array #[tag, lit, dat]
  | Expr.mdata   mdat exp dat =>
    let tag := Ipld.string "MDA"
    let mdat := mDataToIpld mdat
    let dat := Ipld.number dat
    Ipld.array #[tag, mdat, dat]
  | Expr.proj    nam idx exp dat =>
    let tag := Ipld.string "PRO"
    let nam := Ipld.string nam
    let idx := Ipld.byte $ Nat.toByteArrayBE idx
    let exp := exprToIpld exp
    let dat := Ipld.number dat
    Ipld.array #[tag, nam, idx, exp, dat]
