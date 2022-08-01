import Ipld.Multibase
import LSpec

structure Case where
  read: Bool
  multibase : Multibase
  bytes: List UInt8
  string : String

/-- Make a test-case for checking that `case.bytes` encodes to `case.string`
    and that string `y` decodes to `case.bytes` -/
def mkCase (b : Base) (x: List UInt8) (y: String) : Case :=
  ⟨false, b.toMultibase, x, y⟩

/-- Make a test-case for checking that string `y` decodes to `case.bytes`,
    but where `case.bytes` might not encode to `case.string` -/
def mkReadCase (b : Base) (x: List UInt8) (y: String) : Case :=
  ⟨true, b.toMultibase, x, y⟩

/-- Test that a given test-case passes -/
def testCase (case : Case) : Bool := 
  let enc : String := case.multibase.encode case.bytes
  (if case.read then true else enc == case.string)
    && (case.multibase.decode enc == some case.bytes)

def findFailing (cases: List Case) : List Case :=
  cases.filter fun x => not (testCase x)

namespace Basic

def basic : List UInt8 := "yes mani !".toUTF8.data.data

def cases : List Case :=
  [ mkCase .b2 basic "001111001011001010111001100100000011011010110000101101110011010010010000000100001"
  , mkCase .b8 basic             "7362625631006654133464440102"
  , mkCase .b10 basic            "9573277761329450583662625"
  , mkCase .b16 basic            "f796573206d616e692021"
  , mkCase .b16 basic            "f796573206d616e692021"
  , mkCase .b32Hex basic         "vf5in683dc5n6i811"
  , mkCase .b32HexUpper basic    "VF5IN683DC5N6I811"
  , mkCase .b32HexPad basic      "tf5in683dc5n6i811"
  , mkCase .b32HexPadUpper basic "TF5IN683DC5N6I811"
  , mkCase .b32 basic            "bpfsxgidnmfxgsibb"
  , mkCase .b32Upper basic       "BPFSXGIDNMFXGSIBB"
  , mkCase .b32Pad basic         "cpfsxgidnmfxgsibb"
  , mkCase .b32PadUpper basic    "CPFSXGIDNMFXGSIBB"
  , mkCase .b32Z basic           "hxf1zgedpcfzg1ebb"
  , mkCase .b36 basic            "k2lcpzo5yikidynfl"
  , mkCase .b36Upper basic       "K2LCPZO5YIKIDYNFL"
  , mkCase .b58Flickr basic      "Z7Pznk19XTTzBtx"
  , mkCase .b58BTC basic         "z7paNL19xttacUY"
  , mkCase .b64 basic            "meWVzIG1hbmkgIQ"
  , mkCase .b64Pad basic         "MeWVzIG1hbmkgIQ=="
  , mkCase .b64URL basic         "ueWVzIG1hbmkgIQ"
  , mkCase .b64URLPad basic      "UeWVzIG1hbmkgIQ=="
  ]

end Basic

namespace CaseInsensitivity

def hello : List UInt8 := "hello world".toUTF8.data.data

def cases : List Case :=
 [ mkReadCase .b16            hello "f68656c6c6f20776F726C64"
 , mkReadCase .b16Upper       hello "F68656c6c6f20776F726C64"
 , mkReadCase .b32            hello "bnbswy3dpeB3W64TMMQ"
 , mkReadCase .b32Upper       hello "Bnbswy3dpeB3W64TMMQ"
 , mkReadCase .b32Hex         hello "vd1imor3f41RMUSJCCG"
 , mkReadCase .b32HexUpper    hello "Vd1imor3f41RMUSJCCG"
 , mkReadCase .b32Pad         hello "cnbswy3dpeB3W64TMMQ======"
 , mkReadCase .b32PadUpper    hello "Cnbswy3dpeB3W64TMMQ======"
 , mkReadCase .b32HexPad      hello "td1imor3f41RMUSJCCG======"
 , mkReadCase .b32HexPadUpper hello "Td1imor3f41RMUSJCCG======"
 , mkReadCase .b36            hello "kfUvrsIvVnfRbjWaJo"
 , mkReadCase .b36Upper       hello "KfUVrSIVVnFRbJWAJo"
 ]

end CaseInsensitivity

namespace LeadingZero
-- leading_zero.csv

def zero : List UInt8 := "\x00yes mani !".toUTF8.data.data

def cases : List Case :=
  [ mkCase .b2  zero            "00000000001111001011001010111001100100000011011010110000101101110011010010010000000100001"
  , mkCase .b8 zero             "7000745453462015530267151100204"
  , mkCase .b10 zero            "90573277761329450583662625"
  , mkCase .b16 zero            "f00796573206d616e692021"
  , mkCase .b16Upper zero       "F00796573206D616E692021"
  , mkCase .b32 zero            "bab4wk4zanvqw42jaee"
  , mkCase .b32Upper zero       "BAB4WK4ZANVQW42JAEE"
  , mkCase .b32Hex zero         "v01smasp0dlgmsq9044"
  , mkCase .b32HexUpper zero    "V01SMASP0DLGMSQ9044"
  , mkCase .b32Pad zero         "cab4wk4zanvqw42jaee======"
  , mkCase .b32PadUpper zero    "CAB4WK4ZANVQW42JAEE======"
  , mkCase .b32HexPad zero      "t01smasp0dlgmsq9044======"
  , mkCase .b32HexPadUpper zero "T01SMASP0DLGMSQ9044======"
  , mkCase .b32Z zero           "hybhskh3ypiosh4jyrr"
  , mkCase .b36 zero            "k02lcpzo5yikidynfl"
  , mkCase .b36Upper zero       "K02LCPZO5YIKIDYNFL"
  , mkCase .b58Flickr zero      "Z17Pznk19XTTzBtx"
  , mkCase .b58BTC zero         "z17paNL19xttacUY"
  , mkCase .b64 zero            "mAHllcyBtYW5pICE"
  , mkCase .b64Pad zero         "MAHllcyBtYW5pICE="
  , mkCase .b64URL zero         "uAHllcyBtYW5pICE"
  , mkCase .b64URLPad zero      "UAHllcyBtYW5pICE="
  ]

end LeadingZero

namespace TwoLeadingZeros

def zeros : List UInt8 := "\x00\x00yes mani !".toUTF8.data.data

def cases : List Case := 
  [ mkCase .b2 zeros              "0000000000000000001111001011001010111001100100000011011010110000101101110011010010010000000100001"
  , mkCase .b8 zeros              "700000171312714403326055632220041"
  , mkCase .b10 zeros             "900573277761329450583662625"
  , mkCase .b16 zeros             "f0000796573206d616e692021"
  , mkCase .b16Upper zeros        "F0000796573206D616E692021"
  , mkCase .b32 zeros             "baaahszltebwwc3tjeaqq"
  , mkCase .b32Upper zeros        "BAAAHSZLTEBWWC3TJEAQQ"
  , mkCase .b32Hex zeros          "v0007ipbj41mm2rj940gg"
  , mkCase .b32HexUpper zeros     "V0007IPBJ41MM2RJ940GG"
  , mkCase .b32Pad zeros          "caaahszltebwwc3tjeaqq===="
  , mkCase .b32PadUpper zeros     "CAAAHSZLTEBWWC3TJEAQQ===="
  , mkCase .b32HexPad zeros       "t0007ipbj41mm2rj940gg===="
  , mkCase .b32HexPadUpper zeros  "T0007IPBJ41MM2RJ940GG===="
  , mkCase .b32Z zeros            "hyyy813murbssn5ujryoo"
  , mkCase .b36 zeros             "k002lcpzo5yikidynfl"
  , mkCase .b36Upper zeros        "K002LCPZO5YIKIDYNFL"
  , mkCase .b58Flickr zeros       "Z117Pznk19XTTzBtx"
  , mkCase .b58BTC zeros          "z117paNL19xttacUY"
  , mkCase .b64 zeros             "mAAB5ZXMgbWFuaSAh"
  , mkCase .b64Pad zeros          "MAAB5ZXMgbWFuaSAh"
  , mkCase .b64URL zeros          "uAAB5ZXMgbWFuaSAh"
  , mkCase .b64URLPad zeros       "UAAB5ZXMgbWFuaSAh"
  ]

end TwoLeadingZeros

namespace RFC4648
-- RFC4648 Test Vectors: https://datatracker.ietf.org/doc/html/rfc4648#section-10
-- test vector `i` is the first `i` letters of the string "foobar" as UTF8

def foobar := "foobar".toUTF8.data.data

def rfc0 : List UInt8 := foobar.take 0
def rfc1 : List UInt8 := foobar.take 1
def rfc2 : List UInt8 := foobar.take 2
def rfc3 : List UInt8 := foobar.take 3
def rfc4 : List UInt8 := foobar.take 4
def rfc5 : List UInt8 := foobar.take 5
def rfc6 : List UInt8 := foobar.take 6

def cases : List Case :=
  [ mkCase .b16 rfc0  "f"
  , mkCase .b16 rfc1  "f66"
  , mkCase .b16 rfc2  "f666f"
  , mkCase .b16 rfc3  "f666f6f"
  , mkCase .b16 rfc4  "f666f6f62"
  , mkCase .b16 rfc5  "f666f6f6261"
  , mkCase .b16 rfc6  "f666f6f626172"
  , mkCase .b16Upper rfc0 "F"
  , mkCase .b16Upper rfc1 "F66"
  , mkCase .b16Upper rfc2 "F666F"
  , mkCase .b16Upper rfc3 "F666F6F"
  , mkCase .b16Upper rfc4 "F666F6F62"
  , mkCase .b16Upper rfc5 "F666F6F6261"
  , mkCase .b16Upper rfc6 "F666F6F626172"
  , mkCase .b32Hex rfc0 "v"
  , mkCase .b32Hex rfc1 "vco"
  , mkCase .b32Hex rfc2 "vcpng"
  , mkCase .b32Hex rfc3 "vcpnmu"
  , mkCase .b32Hex rfc4 "vcpnmuog"
  , mkCase .b32Hex rfc5 "vcpnmuoj1"
  , mkCase .b32Hex rfc6 "vcpnmuoj1e8"
  , mkCase .b32HexUpper rfc0 "V"
  , mkCase .b32HexUpper rfc1 "VCO"
  , mkCase .b32HexUpper rfc2 "VCPNG"
  , mkCase .b32HexUpper rfc3 "VCPNMU"
  , mkCase .b32HexUpper rfc4 "VCPNMUOG"
  , mkCase .b32HexUpper rfc5 "VCPNMUOJ1"
  , mkCase .b32HexUpper rfc6 "VCPNMUOJ1E8"
  , mkCase .b32HexPad rfc0 "t"
  , mkCase .b32HexPad rfc1 "tco======"
  , mkCase .b32HexPad rfc2 "tcpng===="
  , mkCase .b32HexPad rfc3 "tcpnmu==="
  , mkCase .b32HexPad rfc4 "tcpnmuog="
  , mkCase .b32HexPad rfc5 "tcpnmuoj1"
  , mkCase .b32HexPad rfc6 "tcpnmuoj1e8======"
  , mkCase .b32HexPadUpper rfc0 "T"
  , mkCase .b32HexPadUpper rfc1 "TCO======"
  , mkCase .b32HexPadUpper rfc2 "TCPNG===="
  , mkCase .b32HexPadUpper rfc3 "TCPNMU==="
  , mkCase .b32HexPadUpper rfc4 "TCPNMUOG="
  , mkCase .b32HexPadUpper rfc5 "TCPNMUOJ1"
  , mkCase .b32HexPadUpper rfc6 "TCPNMUOJ1E8======"
  , mkCase .b32 rfc0 "b"
  , mkCase .b32 rfc1 "bmy"
  , mkCase .b32 rfc2 "bmzxq"
  , mkCase .b32 rfc3 "bmzxw6"
  , mkCase .b32 rfc4 "bmzxw6yq"
  , mkCase .b32 rfc5 "bmzxw6ytb"
  , mkCase .b32 rfc6 "bmzxw6ytboi"
  , mkCase .b32Upper rfc0 "B"
  , mkCase .b32Upper rfc1 "BMY"
  , mkCase .b32Upper rfc2 "BMZXQ"
  , mkCase .b32Upper rfc3 "BMZXW6"
  , mkCase .b32Upper rfc4 "BMZXW6YQ"
  , mkCase .b32Upper rfc5 "BMZXW6YTB"
  , mkCase .b32Upper rfc6 "BMZXW6YTBOI"
  , mkCase .b32Pad rfc0 "c"
  , mkCase .b32Pad rfc1 "cmy======"
  , mkCase .b32Pad rfc2 "cmzxq===="
  , mkCase .b32Pad rfc3 "cmzxw6==="
  , mkCase .b32Pad rfc4 "cmzxw6yq="
  , mkCase .b32Pad rfc5 "cmzxw6ytb"
  , mkCase .b32Pad rfc6 "cmzxw6ytboi======"
  , mkCase .b32PadUpper rfc0 "C"
  , mkCase .b32PadUpper rfc1 "CMY======"
  , mkCase .b32PadUpper rfc2 "CMZXQ===="
  , mkCase .b32PadUpper rfc3 "CMZXW6==="
  , mkCase .b32PadUpper rfc4 "CMZXW6YQ="
  , mkCase .b32PadUpper rfc5 "CMZXW6YTB"
  , mkCase .b32PadUpper rfc6 "CMZXW6YTBOI======"
  , mkCase .b64 rfc0 "m"
  , mkCase .b64 rfc1 "mZg"
  , mkCase .b64 rfc2 "mZm8"
  , mkCase .b64 rfc3 "mZm9v"
  , mkCase .b64 rfc4 "mZm9vYg"
  , mkCase .b64 rfc5 "mZm9vYmE"
  , mkCase .b64 rfc6 "mZm9vYmFy"
  , mkCase .b64Pad rfc0 "M"
  , mkCase .b64Pad rfc1 "MZg=="
  , mkCase .b64Pad rfc2 "MZm8="
  , mkCase .b64Pad rfc3 "MZm9v"
  , mkCase .b64Pad rfc4 "MZm9vYg=="
  , mkCase .b64Pad rfc5 "MZm9vYmE="
  , mkCase .b64Pad rfc6 "MZm9vYmFy"
  , mkCase .b64URLPad rfc0 "U"
  , mkCase .b64URLPad rfc1 "UZg=="
  , mkCase .b64URLPad rfc2 "UZm8="
  , mkCase .b64URLPad rfc3 "UZm9v"
  , mkCase .b64URLPad rfc4 "UZm9vYg=="
  , mkCase .b64URLPad rfc5 "UZm9vYmE="
  , mkCase .b64URLPad rfc6 "UZm9vYmFy"
  ]

end RFC4648

open LSpec in
def main := lspecIO $
  test "encodes \"yes mani !\"" (findFailing Basic.cases).isEmpty $
  test "encodes \"hello world\"" (findFailing CaseInsensitivity.cases).isEmpty $
  test "encodes \"\\x00yes mani !\"" (findFailing LeadingZero.cases).isEmpty $
  test "encodes \"\\x00\\x00yes mani !\"" (findFailing TwoLeadingZeros.cases).isEmpty $
  test "encodes vectors" (findFailing RFC4648.cases).isEmpty
