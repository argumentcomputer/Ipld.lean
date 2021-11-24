import Ipld.Utils

class Multibase (β: Type) where
  code  : Char
  alpha : String
  read  : Char → Option Nat
  fix   : Bool
  pad   : Bool

namespace Multibase
  variable (β: Type)
  def zero [Multibase β]: Char := (alpha β)[0]
  def base [Multibase β]: Nat  := (alpha β).length
  def log2Base [Multibase β]: Nat  := Nat.log2' (base β)

  -- This is a little slow
  def digit [Multibase β] (i : Nat): Char :=
    if i >= (alpha β).length then zero β else String.get (alpha β) i

  -- This is very slow
  def read' [Multibase β] (c: Char): Option Nat :=
   let x := String.posOf (alpha β) c
   if x == (alpha β).length then Option.none else Option.some x

  def validDigit [Multibase β] (x: Char): Bool := (read β x) != Option.none
  def validate [Multibase β] (x: String): Bool := List.all x.data (validDigit β)

  def toStringCore [Multibase β]: Nat → Nat → String → String
  | 0, n, str => str
  | fuel+1, 0, str => str
  | fuel+1, n, str =>
    let dig := (digit β (n % (base β)))
    toStringCore fuel (n / (base β)) (String.append (String.singleton dig) str)

  def toString [m: Multibase β]: Nat → String
  | 0 => String.singleton (alpha β)[0]
  | n => toStringCore β (n+1) n ""

  def padRight (input: String) : Nat → String
  | 0 => input
  | n+1 => padRight (String.push input '=') n

  def encode [Multibase β] (x: ByteArray): String :=
    let lzs := ByteArray.leadingZeroBits x
    let log := Multibase.log2Base β
    let left := String.mk (List.replicate (lzs / log) '0')
    let llen := (log - x.size % log)
    let x := if Multibase.fix β then ByteArray.pushZeros x llen else x
    let n := Nat.fromByteArrayBE x
    let str := (Multibase.toStringCore β (n+1) n "")
    let rlen := (log * llen - 1)
    let str := if Multibase.fix β then str.dropRight rlen else str
    let str := if Multibase.fix β && Multibase.pad β then padRight str rlen else str
    String.singleton (Multibase.code β) ++ left ++ str

  --def fromBaseCore [Multibase β]: Nat → Nat → String → Option Nat
  --| 0, acc, input => Option.some acc
  --| fuel+1, acc, "" => Option.some acc
  --| fuel+1, acc, input => do
  --let dig := (fromDigit β input[0])
  --Option.bind dig (fun d =>
  --  fromBaseCore fuel (acc * (base β) + d) (String.drop input 1))

  --def fromBase [m: Multibase β]: String -> Option Nat
  --| "" => Option.none
  --| s => fromBaseCore β (s.length) 0 s

end Multibase

structure Base2
structure Base8
structure Base10
structure Base16
structure Base16Upper
structure Base32Hex
structure Base32HexUpper
structure Base32HexPad
structure Base32HexPadUpper
structure Base32
structure Base32Upper
structure Base32Pad
structure Base32PadUpper
structure Base32Z
structure Base36
structure Base36Upper
structure Base58Btc
structure Base58Flickr
structure Base64
structure Base64Pad
structure Base64Url
structure Base64UrlPad

instance : Multibase Base2 where
  code := '0'
  alpha := "01"
  read
  | '0' => some 0
  | '1' => some 1
  | _   => none
  fix := false
  pad := false

instance : Multibase Base8 where
  code := '7'
  alpha: String := "01234567"
  read : Char → Option Nat
  | '0' => some 0
  | '1' => some 1
  | '2' => some 2
  | '3' => some 3
  | '4' => some 4
  | '5' => some 5
  | '6' => some 6
  | '7' => some 7
  | _ => none
  fix := true
  pad := false

instance : Multibase Base10 where
  code := '9'
  alpha: String := "0123456789"
  read : Char → Option Nat
  | '0' => some 0
  | '1' => some 1
  | '2' => some 2
  | '3' => some 3
  | '4' => some 4
  | '5' => some 5
  | '6' => some 6
  | '7' => some 7
  | '8' => some 8
  | '9' => some 9
  | _ => none
  fix := false
  pad := false

instance : Multibase Base16 where
  code := 'f'
  alpha: String := "0123456789abcdef"
  read : Char → Option Nat
  | '0' => some 0
  | '1' => some 1
  | '2' => some 2
  | '3' => some 3
  | '4' => some 4
  | '5' => some 5
  | '6' => some 6
  | '7' => some 7
  | '8' => some 8
  | '9' => some 9
  | 'a' => some 10
  | 'b' => some 11
  | 'c' => some 12
  | 'd' => some 13
  | 'e' => some 14
  | 'f' => some 15
  | 'A' => some 10
  | 'B' => some 11
  | 'C' => some 12
  | 'D' => some 13
  | 'E' => some 14
  | 'F' => some 15
  | _ => none
  fix := false
  pad := false

namespace Test
  #eval "yes mani !".toUTF8
  def basic := ByteArray.mk #[121, 101, 115, 32, 109, 97, 110, 105, 32, 33]
  set_option maxRecDepth 1000
  #check (rfl : (Multibase.encode Base2 basic) =
  "001111001011001010111001100100000011011010110000101101110011010010010000000100001")
  --#eval (Multibase.encode Base8 basic)
  #check (rfl : (Multibase.encode Base8 basic) = "7362625631006654133464440102")
  #check (rfl : (Multibase.encode Base10 basic) = "9573277761329450583662625")
  #check (rfl : (Multibase.encode Base16 basic) = "f796573206d616e692021")
end Test


--  namespace Test
--    private def base2ex1 : String := encode base2 ({ data := #[0x58, 0x59, 0x5a]})
--    #eval base2ex1 == "0010110000101100101011010"
--    private def base2ex2 : String := encode base2 ({ data := #[0x1a]})
--    #eval base2ex2 == "000011010"
--
--    private def base10ex1 : String := encode base10 ({ data := #[0x00, 0x01]})
--    #eval base10ex1 == "901"
--    private def base10ex2 : String := encode base10 ({ data := #[0x00, 0x00, 0xff]})
--    #eval base10ex2 == "900255"
--    private def base10ex3 : String := encode base10 ({ data := #[0x01, 0x00]})
--    #eval base10ex3 == "9256"
--    private def base10ex4 : String := encode base10 ({ data := #[0x00, 0x01, 0x00]})
--    #eval base10ex4 == "90256"
--
--    -- basic.csv
--    def basic : ByteArray := "yes mani !".toUTF8
--    #eval basic == ByteArray.mk #[0x79, 0x65, 0x73, 0x20, 0x6D, 0x61, 0x6E, 0x69, 0x20, 0x21]
--
--    #eval encode base2 basic == 
--      "001111001011001010111001100100000011011010110000101101110011010010010000000100001"
--    #eval encode base8 basic -- == "7362625631006654133464440102"
--
--    #eval encode base10            basic == "9573277761329450583662625"
--    #eval encode base16            basic == "f796573206d616e692021"
--    #eval encode base16upper       basic == "F796573206D616E692021"
--    #eval encode base32            basic == "bpfsxgidnmfxgsibb"
--    #eval encode base32upper       basic == "BPFSXGIDNMFXGSIBB"
--    #eval encode base32hex         basic == "vf5in683dc5n6i811"
--    #eval encode base32hexupper    basic == "VF5IN683DC5N6I811"
--    #eval encode base32pad         basic == "cpfsxgidnmfxgsibb"
--    #eval encode base32padupper    basic == "CPFSXGIDNMFXGSIBB"
--    #eval encode base32hexpad      basic == "tf5in683dc5n6i811"
--    #eval encode base32hexpadupper basic == "TF5IN683DC5N6I811"
--    #eval encode base32z           basic == "hxf1zgedpcfzg1ebb"
--    #eval encode base36            basic == "k2lcpzo5yikidynfl"
--    #eval encode base36upper       basic == "K2LCPZO5YIKIDYNFL"
--    #eval encode base58flickr      basic == "Z7Pznk19XTTzBtx"
--    #eval encode base58btc         basic == "z7paNL19xttacUY"
--    #eval encode base64            basic == "meWVzIG1hbmkgIQ"
--    #eval encode base64pad         basic == "MeWVzIG1hbmkgIQ=="
--    #eval encode base64url         basic == "ueWVzIG1hbmkgIQ"
--    #eval encode base64urlpad      basic == "UeWVzIG1hbmkgIQ=="
--
--    #eval "MA".toUTF8
--    #eval encode base64 {data := #[0x4d, 0x61, 0x00] }
--    #eval Nat.fromByteArrayBE "MA".toUTF8
--    #eval encode base64 "Man".toUTF8
--    #eval encode base64 "yes".toUTF8
--    #eval encode base64 "A".toUTF8 == "mQQ"
--    #eval encode base64 "AA".toUTF8 == "mQUE"
--    #eval encode base64 "AAA".toUTF8 == "mQUFB"
--
---- RFC4648 Test Vectors: https://datatracker.ietf.org/doc/html/rfc4648#section-10
--    #eval encode base64pad "".toUTF8       == ""
--    #eval encode base64pad "f".toUTF8      == "MZg=="
--    #eval encode base64pad "fo".toUTF8     == "MZm8g="
--    #eval encode base64pad "foo".toUTF8    == "MZm9v"
--    #eval encode base64pad "foob".toUTF8   == "MZm9vYg=="
--    #eval encode base64pad "fooba".toUTF8  == "MZm9vYmE="
--    #eval encode base64pad "foobar".toUTF8 == "MZm9vYmFy"
--
--    #eval encode base32padupper "".toUTF8       == ""
--    #eval encode base32padupper "f".toUTF8      == "CMY======"
--    #eval encode base32padupper "fo".toUTF8     == "CMZXQ===="
--    #eval encode base32padupper "foo".toUTF8    == "CMZXW6==="
--    #eval encode base32padupper "foob".toUTF8   == "CMZXW6YQ="
--    #eval encode base32padupper "fooba".toUTF8  == "CMZXW6YTB"
--    #eval encode base32padupper "foobar".toUTF8 == "CMZXW6YTBOI======"
--
--    #eval encode base32hexpadupper "".toUTF8       == ""
--    #eval encode base32hexpadupper "f".toUTF8      == "TCO======"
--    #eval encode base32hexpadupper "fo".toUTF8     == "TCPNG===="
--    #eval encode base32hexpadupper "foo".toUTF8    == "TCPNMU==="
--    #eval encode base32hexpadupper "foob".toUTF8   == "TCPNMUOG="
--    #eval encode base32hexpadupper "fooba".toUTF8  == "TCPNMUOJ1"
--    #eval encode base32hexpadupper "fooba".toUTF8  == "TCPNMUOJ1"
--    #eval encode base32hexpadupper "foobar".toUTF8 == "TCPNMUOJ1E8======"
--
--    #eval encode base16upper "".toUTF8       == ""
--    #eval encode base16upper "f".toUTF8      == "F66"
--    #eval encode base16upper "fo".toUTF8     == "F666F"
--    #eval encode base16upper "foo".toUTF8    == "F666F6F"
--    #eval encode base16upper "foob".toUTF8   == "F666F6F62"
--    #eval encode base16upper "fooba".toUTF8  == "F666F6F6261"
--    #eval encode base16upper "foobar".toUTF8 == "F666F6F626172"
--
--    #eval encode base16 "hello world".toUTF8 == "f68656c6c6f20776f726c64"
--    #eval encode base16upper "hello world".toUTF8 == "F68656C6C6F20776F726C64"
--    #eval encode base32 "hello world".toUTF8 == "bnbswy3dpeb3w64tmmq"
--    #eval encode base32upper "hello world".toUTF8 -- == "F68656C6C6F20776F726C64"
--    #eval encode base36 "hello world".toUTF8 == "kfuvrsivvnfrbjwajo"
--
--    


