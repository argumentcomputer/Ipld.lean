import Ipld.Utils

def toVarIntCore : Nat → Nat → List UInt8
| 0, n => []
| fuel+1, n =>
  let b: UInt8 := UInt8.ofNat (n % 128);
  let n' := n / 128;
  if n' = 0 then [b]
  else (b + 128) :: (toVarIntCore fuel n')

def toVarInt (n: Nat) : List UInt8 := 
  toVarIntCore (n+1) n 

def readVarIntCore : Nat → Nat → List UInt8 → Nat → Option (Nat × List UInt8)
| 0, i, bs, n => some (n, bs)
| fuel+1, i, [], n => none
| fuel+1, i, b::bs, n => 
  let b' := Nat.shiftLeft (UInt8.toNat b % 128) (i * 7)
  if b / 128 == 0 then (n + b', bs) else readVarIntCore fuel (i + 1) bs (n + b')

def readVarInt (b: List UInt8) : Option (Nat × List UInt8) :=
  readVarIntCore b.length 0 b 0

instance : BEq ByteArray where
  beq a b := a.data == b.data


namespace Test

theorem encode1 : toVarInt 1 = [0b00000001] := rfl
theorem read1 : readVarInt  [0b00000001] = some (1, []) := rfl
theorem encode127 : toVarInt 127 =  [0b01111111] := rfl
theorem read127 : readVarInt  [0b01111111] = some (127, []) := rfl
theorem encode128 : toVarInt 128 =  [0b10000000, 0b00000001] := rfl
theorem read128 : readVarInt  [0b10000000, 0b00000001] = some (128, []) := rfl
theorem encode300 : toVarInt 300 =  [0b10101100, 0b00000010] := rfl
theorem read300 : readVarInt  [0b10101100, 0b00000010] = some (300, []) := rfl
theorem encode16384 : toVarInt 16384 =  [0b10000000, 0b10000000, 0b00000001] := rfl
theorem read16384 : readVarInt  [0b10000000, 0b10000000, 0b00000001] = some (16384, []) := rfl
end Test
