import Ipld.UnsignedVarInt
import LSpec

structure Case where
  nat: Nat
  bytes: ByteArray

/-- Test that a given test-case passes -/
def testCase (case : Case) : Bool := 
  match UnsignedVarInt.fromVarInt case.bytes with
  | Option.none => false
  | Option.some (n,_) =>
    UnsignedVarInt.toVarInt case.nat == case.bytes && n == case.nat

def findFailing (cases: List Case) : List Case :=
  List.filter (fun x => not (testCase x)) cases

def cases : List Case := 
  [ Case.mk 1   { data := #[0b00000001] }
  , Case.mk 127 { data := #[0b01111111] }
  , Case.mk 128 { data := #[0b10000000, 0b00000001] }
  , Case.mk 255 { data := #[0b11111111, 0b00000001] }
  , Case.mk 300 { data := #[0b10101100, 0b00000010] }
  , Case.mk 16384 { data := #[0b10000000, 0b10000000, 0b000000001] }
  ]

open LSpec in
def main := lspecIO $
  test "converts `Nat` ↔ `UnsignedVarInt` properly" (findFailing cases).isEmpty
