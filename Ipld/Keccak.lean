namespace ByteArray

def toArrayUInt64LE (bytes : ByteArray) : Array UInt64 :=
  let push_word := λ (pos, word, arr) byte =>
    if pos < 7
    then
      let word := word + (byte.toUInt64 <<< (8*pos))
      (pos+1, word, arr)
    else
      let word := word + (byte.toUInt64 <<< (8*pos))
      let arr := arr.push word
      (0, 0, arr)
  let pos : UInt64 := 0
  let word : UInt64 := 0
  let (pos, word, arr) := foldl push_word (pos, word, Array.empty) bytes
  if pos == 0
  then arr
  else arr.push word

def fromArrayUInt64LE (arr : Array UInt64) : ByteArray :=
  let push_word := λ bytes word =>
    let bytes := bytes.push $ UInt64.toUInt8 word
    let bytes := bytes.push $ UInt64.toUInt8 $ word >>> 0x8
    let bytes := bytes.push $ UInt64.toUInt8 $ word >>> 0x10
    let bytes := bytes.push $ UInt64.toUInt8 $ word >>> 0x18
    let bytes := bytes.push $ UInt64.toUInt8 $ word >>> 0x20
    let bytes := bytes.push $ UInt64.toUInt8 $ word >>> 0x28
    let bytes := bytes.push $ UInt64.toUInt8 $ word >>> 0x30
    let bytes := bytes.push $ UInt64.toUInt8 $ word >>> 0x38
    bytes
  Array.foldl push_word empty arr

end ByteArray

namespace Keccak

def rounds : Nat :=
  24

def numLanes : Nat :=
  25

def laneWidth : Nat :=
  64

def emptyState : Array UInt64 :=
  mkArray numLanes 0

----------------------------------------------------
-- Constants used in KeccakF[1600] permutation
----------------------------------------------------

def roundConstants : Array UInt64 := {
  data := [ 0x0000000000000001, 0x0000000000008082, 0x800000000000808A
          , 0x8000000080008000, 0x000000000000808B, 0x0000000080000001
          , 0x8000000080008081, 0x8000000000008009, 0x000000000000008A
          , 0x0000000000000088, 0x0000000080008009, 0x000000008000000A
          , 0x000000008000808B, 0x800000000000008B, 0x8000000000008089
          , 0x8000000000008003, 0x8000000000008002, 0x8000000000000080
          , 0x000000000000800A, 0x800000008000000A, 0x8000000080008081
          , 0x8000000000008080, 0x0000000080000001, 0x8000000080008008 ]
}

def rotationConstants : Array UInt64 := {
  data := [  0, 36,  3, 41, 18
          ,  1, 44, 10, 45,  2
          , 62,  6, 43, 15, 61
          , 28, 55, 25, 21, 56
          , 27, 20, 39,  8, 14 ]
}

def piConstants : Array Nat := {
  data := [ 0, 15, 5, 20, 10
          , 6, 21, 11, 1, 16
          , 12, 2, 17, 7, 22
          , 18, 8, 23, 13, 3
          , 24, 14, 4, 19, 9 ]
}

def theta (state : Array UInt64) : Array UInt64 :=
  let b : Array UInt64  := Array.mkArray 5 0
  let c := Array.mapIdx b (λ i _ => UInt64.xor state[i*5] $ UInt64.xor state[i*5+1] $ UInt64.xor state[i*5+2] $ UInt64.xor state[i*5+3] state[i*5+4])
  let d := Array.mapIdx b (λ i _ => (i, UInt64.xor c[Nat.mod (i + 4) 5] (UInt64.shiftLeft c[Nat.mod (i + 1) 5] 1)))
  Array.concatMap (λ (i, e) => { data := [UInt64.xor e state[i*5], UInt64.xor e state[i*5+1], UInt64.xor e state[i*5+2], UInt64.xor e state[i*5+3], UInt64.xor e state[i*5+4]]}) d

def rho (state : Array UInt64) : Array UInt64 := Array.zipWith state rotationConstants UInt64.shiftLeft

def pi (state : Array UInt64) : Array UInt64 :=
  Array.map (λ i => state[i]) piConstants

def chi (b : Array UInt64) : Array UInt64 :=
  Array.mapIdx b (λ z el => UInt64.xor el (UInt64.complement (UInt64.land b[Nat.mod (z + 5) 25] b[Nat.mod (z + 10) 25])))

def iota (roundNumber : Nat) (state : Array UInt64) : Array UInt64 :=
  Array.modify state 0 (λ val => UInt64.xor roundConstants[roundNumber] val)

def keccakFAux : Nat → Nat → Array UInt64 → (Nat × Array UInt64)
| 0, r, s => (r, s)
| n+1, r, s => keccakFAux n (r+1) (iota r $ chi $ pi $ rho $ theta s)

def keccakF (state : Array UInt64) : Array UInt64 :=
  let (_, state') := keccakFAux rounds 0 state
  state'

def squeeze (rate : Nat) (l : Nat) (state : Array UInt64) : ByteArray :=
  let lanesToExtract := Nat.div l (Nat.div laneWidth 8)
  let threshold := Nat.div rate laneWidth
  let rec stateToBytes : Nat → Nat → Nat → Array UInt64 → List UInt64
  | 0, _, _, _ => []
  | n+1, rate, x, s =>
    if x < threshold
    then List.cons s[(Nat.div x 5) + (Nat.mod x 5) * 5] (stateToBytes n rate (x+1) s)
    else stateToBytes n rate 0 (keccakF s)
  ByteArray.fromArrayUInt64LE { data := stateToBytes lanesToExtract threshold 0 state }


partial def absorbBlock (rate : Nat) (state : Array UInt64) (input : Array UInt64) : Array UInt64 :=
  if Array.isEmpty input
  then state
  else
    let threshold := Nat.div rate laneWidth
    let state' := Array.mapIdx state (λ z el =>
      if (Nat.div z 5) + 5*(Nat.mod z 5) < threshold
      then UInt64.xor el input[(Nat.div z 5) + 5*(Nat.mod z 5)]
      else el)
    let input := { data := List.drop (Nat.div rate 64) input.data }
    absorbBlock rate (keccakF state') input


def absorb (rate : Nat) (bytes : ByteArray) : Array UInt64 :=
  absorbBlock rate emptyState (ByteArray.toArrayUInt64LE bytes)

end Keccak
