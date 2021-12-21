import Ipld.Multibase
namespace Multibase.Test
  set_option maxHeartbeats 0
  set_option maxRecDepth 1500

namespace Basic
#eval "yes mani !".toUTF8
def basic : List UInt8 := [121, 101, 115, 32, 109, 97, 110, 105, 32, 33]
theorem encodeBase2             : encode Base2 basic             = "001111001011001010111001100100000011011010110000101101110011010010010000000100001" := rfl
theorem encodeBase8             : encode Base8 basic             = "7362625631006654133464440102" := rfl
theorem encodeBase10            : encode Base10 basic            = "9573277761329450583662625"    := rfl
theorem encodeBase16            : encode Base16 basic            = "f796573206d616e692021"        := rfl
theorem encodeBase16Upper       : encode Base16 basic            = "f796573206d616e692021"        := rfl
theorem encodeBase32Hex         : encode Base32Hex basic         = "vf5in683dc5n6i811"            := rfl
theorem encodeBase32HexUpper    : encode Base32HexUpper basic    = "VF5IN683DC5N6I811"            := rfl
theorem encodeBase32HexPad      : encode Base32HexPad basic      = "tf5in683dc5n6i811"            := rfl
theorem encodeBase32HexPadUpper : encode Base32HexPadUpper basic = "TF5IN683DC5N6I811"            := rfl
theorem encodeBase32            : encode Base32 basic            = "bpfsxgidnmfxgsibb"            := rfl
theorem encodeBase32Upper       : encode Base32Upper basic       = "BPFSXGIDNMFXGSIBB"            := rfl
theorem encodeBase32Pad         : encode Base32Pad basic         = "cpfsxgidnmfxgsibb"            := rfl
theorem encodeBase32PadUpper    : encode Base32PadUpper basic    = "CPFSXGIDNMFXGSIBB"            := rfl
theorem encodeBase32Z           : encode Base32Z basic           = "hxf1zgedpcfzg1ebb"            := rfl
theorem encodeBase36            : encode Base36 basic            = "k2lcpzo5yikidynfl"            := rfl
theorem encodeBase36Upper       : encode Base36Upper basic       = "K2LCPZO5YIKIDYNFL"            := rfl
theorem encodeBase58Flickr      : encode Base58Flickr basic      = "Z7Pznk19XTTzBtx"              := rfl
theorem encodeBase58BTC         : encode Base58BTC basic         = "z7paNL19xttacUY"              := rfl
theorem encodeBase64            : encode Base64 basic            = "meWVzIG1hbmkgIQ"              := rfl
theorem encodeBase64Pad         : encode Base64Pad basic         = "MeWVzIG1hbmkgIQ=="            := rfl
theorem encodeBaseURL           : encode Base64URL basic         = "ueWVzIG1hbmkgIQ"              := rfl
theorem encodeBaseURLPad        : encode Base64URLPad basic      = "UeWVzIG1hbmkgIQ=="            := rfl

-- These tests are really slow in the kernel
#eval decode Base2
"001111001011001010111001100100000011011010110000101101110011010010010000000100001" == (some basic)
--theorem decodeBase2 : decode Base2 "001111001011001010111001100100000011011010110000101101110011010010010000000100001" = (some basic) := rfl
#eval decode Base8 "7362625631006654133464440102" == (some basic)
--theorem decodeBase8           : decode Base8             "7362625631006654133464440102 "  = (some basic) := rfl

theorem decodeBase10            : decode Base10            "9573277761329450583662625" = some basic := rfl
theorem decodeBase16            : decode Base16            "f796573206d616e692021"     = some basic := rfl
theorem decodeBase16Upper       : decode Base16Upper       "F796573206D616E692021"     = some basic := rfl
theorem decodeBase32Hex         : decode Base32Hex         "vf5in683dc5n6i811"         = some basic := rfl
theorem decodeBase32HexUpper    : decode Base32HexUpper    "VF5IN683DC5N6I811"         = some basic := rfl
theorem decodeBase32HexPad      : decode Base32HexPad      "tf5in683dc5n6i811"         = some basic := rfl
theorem decodeBase32HexPadUpper : decode Base32HexPadUpper "TF5IN683DC5N6I811"         = some basic := rfl
theorem decodeBase32            : decode Base32            "bpfsxgidnmfxgsibb"         = some basic := rfl
theorem decodeBase32Upper       : decode Base32Upper       "BPFSXGIDNMFXGSIBB"         = some basic := rfl
theorem decodeBase32Pad         : decode Base32Pad         "cpfsxgidnmfxgsibb"         = some basic := rfl
theorem decodeBase32PadUpper    : decode Base32PadUpper    "CPFSXGIDNMFXGSIBB"         = some basic := rfl
theorem decodeBase32Z           : decode Base32Z           "hxf1zgedpcfzg1ebb"         = some basic := rfl
theorem decodeBase36            : decode Base36            "k2lcpzo5yikidynfl"         = some basic := rfl
theorem decodeBase36Upper       : decode Base36Upper       "K2LCPZO5YIKIDYNFL"         = some basic := rfl
theorem decodeBase58Flickr      : decode Base58Flickr      "Z7Pznk19XTTzBtx"           = some basic := rfl
theorem decodeBase58BTC         : decode Base58BTC         "z7paNL19xttacUY"           = some basic := rfl
theorem decodeBase64            : decode Base64            "meWVzIG1hbmkgIQ"           = some basic := rfl
theorem decodeBase64Pad         : decode Base64Pad         "MeWVzIG1hbmkgIQ=="         = some basic := rfl
theorem decodeBaseURL           : decode Base64URL         "ueWVzIG1hbmkgIQ"           = some basic := rfl
theorem decodeBaseURLPad        : decode Base64URLPad      "UeWVzIG1hbmkgIQ=="         = some basic := rfl
end Basic

namespace CaseInsensitivity
#eval "hello world".toUTF8
def hello : List UInt8 := [104, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100]

theorem caseBase16           : decode Base16                 "f68656c6c6f20776F726C64"   = some hello  := rfl
theorem caseBase16Upper      : decode Base16Upper            "F68656c6c6f20776F726C64"   = some hello  := rfl
theorem caseBase32           : decode Base32                 "bnbswy3dpeB3W64TMMQ"       = some hello  := rfl
theorem caseBase32Upper      : decode Base32Upper            "Bnbswy3dpeB3W64TMMQ"       = some hello  := rfl
theorem caseBase32Hex        : decode Base32Hex              "vd1imor3f41RMUSJCCG"       = some hello  := rfl
theorem caseBase32HexUpper   : decode Base32HexUpper         "Vd1imor3f41RMUSJCCG"       = some hello  := rfl
theorem caseBase32Pad        : decode Base32Pad              "cnbswy3dpeB3W64TMMQ======" = some hello  := rfl
theorem caseBase32PadUpper   : decode Base32PadUpper         "Cnbswy3dpeB3W64TMMQ======" = some hello  := rfl
theorem caseBase32HexPad     : decode Base32HexPad           "td1imor3f41RMUSJCCG======" = some hello  := rfl
theorem caseBase32HexPadUpper: decode Base32HexPadUpper      "Td1imor3f41RMUSJCCG======" = some hello  := rfl
theorem caseBase36           : decode Base36                 "kfUvrsIvVnfRbjWaJo"        = some hello  := rfl
theorem caseBase36Upper      : decode Base36Upper            "KfUVrSIVVnFRbJWAJo"        = some hello  := rfl

end CaseInsensitivity

namespace LeadingZero
-- leading_zero.csv
#eval "\x00yes mani !".toUTF8
def zero : List UInt8 := [0, 121, 101, 115, 32, 109, 97, 110, 105, 32, 33]

theorem encodeBase2             : encode Base2  zero            = "00000000001111001011001010111001100100000011011010110000101101110011010010010000000100001" := rfl
theorem encodeBase8             : encode Base8 zero             = "7000745453462015530267151100204" := rfl
theorem encodeBase10            : encode Base10 zero            = "90573277761329450583662625"      := rfl
theorem encodeBase16            : encode Base16 zero            = "f00796573206d616e692021"         := rfl
theorem encodeBase16Upper       : encode Base16Upper zero       = "F00796573206D616E692021"         := rfl
theorem encodeBase32            : encode Base32 zero            = "bab4wk4zanvqw42jaee"             := rfl
theorem encodeBase32Upper       : encode Base32Upper zero       = "BAB4WK4ZANVQW42JAEE"             := rfl
theorem encodeBase32Hex         : encode Base32Hex zero         = "v01smasp0dlgmsq9044"             := rfl
theorem encodeBase32HexUpper    : encode Base32HexUpper zero    = "V01SMASP0DLGMSQ9044"             := rfl
theorem encodeBase32Pad         : encode Base32Pad zero         = "cab4wk4zanvqw42jaee======"       := rfl
theorem encodeBase32PadUpper    : encode Base32PadUpper zero    = "CAB4WK4ZANVQW42JAEE======"       := rfl
theorem encodeBase32HexPad      : encode Base32HexPad zero      = "t01smasp0dlgmsq9044======"       := rfl
theorem encodeBase32HexPadUpper : encode Base32HexPadUpper zero = "T01SMASP0DLGMSQ9044======"       := rfl
theorem encodeBase32Z           : encode Base32Z zero           = "hybhskh3ypiosh4jyrr"             := rfl
theorem encodeBase36            : encode Base36 zero            = "k02lcpzo5yikidynfl"              := rfl
theorem encodeBase36Upper       : encode Base36Upper zero       = "K02LCPZO5YIKIDYNFL"              := rfl
theorem encodeBase58Flickr      : encode Base58Flickr zero      = "Z17Pznk19XTTzBtx"                := rfl
theorem encodeBase58BTC         : encode Base58BTC zero         = "z17paNL19xttacUY"                := rfl
theorem encodeBase64            : encode Base64 zero            = "mAHllcyBtYW5pICE"                := rfl
theorem encodeBase64Pad         : encode Base64Pad zero         = "MAHllcyBtYW5pICE="               := rfl
theorem encodeBase64URL         : encode Base64URL zero         = "uAHllcyBtYW5pICE"                := rfl
theorem encodeBase64URLPad      : encode Base64URLPad zero      = "UAHllcyBtYW5pICE="               := rfl

-- Slow in kernel
#eval decode Base2 "00000000001111001011001010111001100100000011011010110000101101110011010010010000000100001" == (some zero)
#eval decode Base8 "7000745453462015530267151100204" == (some zero)
--theorem decodeBase2
--  : decode Base2  "00000000001111001011001010111001100100000011011010110000101101110011010010010000000100001" = (some zero)
--  := rfl
--theorem decodeBase8 
--  : decode Base8 "7000745453462015530267151100204" = (some zero)
--  := rfl

theorem decodeBase10            : decode Base10            "90573277761329450583662625" = some zero := rfl
theorem decodeBase16            : decode Base16            "f00796573206d616e692021"    = some zero := rfl
theorem decodeBase16Upper       : decode Base16Upper       "F00796573206D616E692021"    = some zero := rfl
theorem decodeBase32            : decode Base32            "bab4wk4zanvqw42jaee"        = some zero := rfl
theorem decodeBase32Upper       : decode Base32Upper       "BAB4WK4ZANVQW42JAEE"        = some zero := rfl
theorem decodeBase32Hex         : decode Base32Hex         "v01smasp0dlgmsq9044"        = some zero := rfl
theorem decodeBase32HexUpper    : decode Base32HexUpper    "V01SMASP0DLGMSQ9044"        = some zero := rfl
theorem decodeBase32Pad         : decode Base32Pad         "cab4wk4zanvqw42jaee======"  = some zero := rfl
theorem decodeBase32PadUpper    : decode Base32PadUpper    "CAB4WK4ZANVQW42JAEE======"  = some zero := rfl
theorem decodeBase32HexPad      : decode Base32HexPad      "t01smasp0dlgmsq9044======"  = some zero := rfl
theorem decodeBase32HexPadUpper : decode Base32HexPadUpper "T01SMASP0DLGMSQ9044======"  = some zero := rfl
theorem decodeBase32Z           : decode Base32Z           "hybhskh3ypiosh4jyrr"        = some zero := rfl
theorem decodeBase36            : decode Base36            "k02lcpzo5yikidynfl"         = some zero := rfl
theorem decodeBase36Upper       : decode Base36Upper       "K02LCPZO5YIKIDYNFL"         = some zero := rfl
theorem decodeBase58Flickr      : decode Base58Flickr      "Z17Pznk19XTTzBtx"           = some zero := rfl
theorem decodeBase58BTC         : decode Base58BTC         "z17paNL19xttacUY"           = some zero := rfl
theorem decodeBase64            : decode Base64            "mAHllcyBtYW5pICE"           = some zero := rfl
theorem decodeBase64Pad         : decode Base64Pad         "MAHllcyBtYW5pICE="          = some zero := rfl
theorem decodeBase64URL         : decode Base64URL         "uAHllcyBtYW5pICE"           = some zero := rfl
theorem decodeBase64URLPad      : decode Base64URLPad      "UAHllcyBtYW5pICE="          = some zero := rfl
end LeadingZero

namespace TwoLeadingZeros
  -- two_leading_zeros.csv
  #eval "\x00\x00yes mani !".toUTF8
  def zeros : List UInt8 := [0, 0, 121, 101, 115, 32, 109, 97, 110, 105, 32, 33]

theorem encodeBase2            : encode Base2 zeros              = "0000000000000000001111001011001010111001100100000011011010110000101101110011010010010000000100001" := rfl
theorem encodeBase8            : encode Base8 zeros              = "700000171312714403326055632220041" := rfl
theorem encodeBase10           : encode Base10 zeros             = "900573277761329450583662625"       := rfl
theorem encodeBase16           : encode Base16 zeros             = "f0000796573206d616e692021"         := rfl
theorem encodeBase16Upper      : encode Base16Upper zeros        = "F0000796573206D616E692021"         := rfl
theorem encodeBase32           : encode Base32 zeros             = "baaahszltebwwc3tjeaqq"             := rfl
theorem encodeBase32Upper      : encode Base32Upper zeros        = "BAAAHSZLTEBWWC3TJEAQQ"             := rfl
theorem encodeBase32Hex        : encode Base32Hex zeros          = "v0007ipbj41mm2rj940gg"             := rfl
theorem encodeBase32HexUpper   : encode Base32HexUpper zeros     = "V0007IPBJ41MM2RJ940GG"             := rfl
theorem encodeBase32Pad        : encode Base32Pad zeros          = "caaahszltebwwc3tjeaqq===="         := rfl
theorem encodeBase32PadUpper   : encode Base32PadUpper zeros     = "CAAAHSZLTEBWWC3TJEAQQ===="         := rfl
theorem encodeBase32HexPad     : encode Base32HexPad zeros       = "t0007ipbj41mm2rj940gg===="         := rfl
theorem encodeBase32HexPadUpper: encode Base32HexPadUpper zeros  = "T0007IPBJ41MM2RJ940GG===="         := rfl
theorem encodeBase32Z          : encode Base32Z zeros            = "hyyy813murbssn5ujryoo"             := rfl
theorem encodeBase36           : encode Base36 zeros             = "k002lcpzo5yikidynfl"               := rfl
theorem encodeBase36Upper      : encode Base36Upper zeros        = "K002LCPZO5YIKIDYNFL"               := rfl
theorem encodeBase58Flickr     : encode Base58Flickr zeros       = "Z117Pznk19XTTzBtx"                 := rfl
theorem encodeBase58BTC        : encode Base58BTC zeros          = "z117paNL19xttacUY"                 := rfl
theorem encodeBase64           : encode Base64 zeros             = "mAAB5ZXMgbWFuaSAh"                 := rfl
theorem encodeBase64Pad        : encode Base64Pad zeros          = "MAAB5ZXMgbWFuaSAh"                 := rfl
theorem encodeBase64URL        : encode Base64URL zeros          = "uAAB5ZXMgbWFuaSAh"                 := rfl
theorem encodeBase64URLPad     : encode Base64URLPad zeros       = "UAAB5ZXMgbWFuaSAh"                 := rfl

--theorem decodeBase2            : decode Base2 "0000000000000000001111001011001010111001100100000011011010110000101101110011010010010000000100001" = some zeros := rfl
--theorem decodeBase8            : decode Base8             "700000171312714403326055632220041"  = some zeros := rfl

theorem decodeBase10           : decode Base10            "900573277761329450583662625"        = some zeros := rfl
theorem decodeBase16           : decode Base16            "f0000796573206d616e692021"          = some zeros := rfl
theorem decodeBase16Upper      : decode Base16Upper       "F0000796573206D616E692021"          = some zeros := rfl
theorem decodeBase32           : decode Base32            "baaahszltebwwc3tjeaqq"              = some zeros := rfl
theorem decodeBase32Upper      : decode Base32Upper       "BAAAHSZLTEBWWC3TJEAQQ"              = some zeros := rfl
theorem decodeBase32Hex        : decode Base32Hex         "v0007ipbj41mm2rj940gg"              = some zeros := rfl
theorem decodeBase32HexUpper   : decode Base32HexUpper    "V0007IPBJ41MM2RJ940GG"              = some zeros := rfl
theorem decodeBase32Pad        : decode Base32Pad         "caaahszltebwwc3tjeaqq===="          = some zeros := rfl
theorem decodeBase32PadUpper   : decode Base32PadUpper    "CAAAHSZLTEBWWC3TJEAQQ===="          = some zeros := rfl
theorem decodeBase32HexPad     : decode Base32HexPad      "t0007ipbj41mm2rj940gg===="          = some zeros := rfl
theorem decodeBase32HexPadUpper: decode Base32HexPadUpper "T0007IPBJ41MM2RJ940GG===="          = some zeros := rfl
theorem decodeBase32Z          : decode Base32Z           "hyyy813murbssn5ujryoo"              = some zeros := rfl
theorem decodeBase36           : decode Base36            "k002lcpzo5yikidynfl"                = some zeros := rfl
theorem decodeBase36Upper      : decode Base36Upper       "K002LCPZO5YIKIDYNFL"                = some zeros := rfl
theorem decodeBase58Flickr     : decode Base58Flickr      "Z117Pznk19XTTzBtx"                  = some zeros := rfl
theorem decodeBase58BTC        : decode Base58BTC         "z117paNL19xttacUY"                  = some zeros := rfl
theorem decodeBase64           : decode Base64            "mAAB5ZXMgbWFuaSAh"                  = some zeros := rfl
theorem decodeBase64Pad        : decode Base64Pad         "MAAB5ZXMgbWFuaSAh"                  = some zeros := rfl
theorem decodeBase64URL        : decode Base64URL         "uAAB5ZXMgbWFuaSAh"                  = some zeros := rfl
theorem decodeBase64URLPad     : decode Base64URLPad      "UAAB5ZXMgbWFuaSAh"                  = some zeros := rfl
end TwoLeadingZeros

namespace RFC4648
-- RFC4648 Test Vectors: https://datatracker.ietf.org/doc/html/rfc4648#section-10
-- test vector `i` is the first `i` letters of the string "foobar" as UTF8
#eval "foobar".toUTF8
def rfc0 : List UInt8 := []
def rfc1 : List UInt8 := [102]
def rfc2 : List UInt8 := [102, 111]
def rfc3 : List UInt8 := [102, 111, 111]
def rfc4 : List UInt8 := [102, 111, 111, 98]
def rfc5 : List UInt8 := [102, 111, 111, 98, 97]
def rfc6 : List UInt8 := [102, 111, 111, 98, 97, 114]

theorem encodeBase16Rfc0 : encode Base16 rfc0 = "f"             := rfl
theorem encodeBase16Rfc1 : encode Base16 rfc1 = "f66"           := rfl
theorem encodeBase16Rfc2 : encode Base16 rfc2 = "f666f"         := rfl
theorem encodeBase16Rfc3 : encode Base16 rfc3 = "f666f6f"       := rfl
theorem encodeBase16Rfc4 : encode Base16 rfc4 = "f666f6f62"     := rfl
theorem encodeBase16Rfc5 : encode Base16 rfc5 = "f666f6f6261"   := rfl
theorem encodeBase16Rfc6 : encode Base16 rfc6 = "f666f6f626172" := rfl

theorem decodeBase16Rfc0 : decode Base16 "f"             = some rfc0       := rfl
theorem decodeBase16Rfc1 : decode Base16 "f66"           = some rfc1       := rfl
theorem decodeBase16Rfc2 : decode Base16 "f666f"         = some rfc2       := rfl
theorem decodeBase16Rfc3 : decode Base16 "f666f6f"       = some rfc3       := rfl
theorem decodeBase16Rfc4 : decode Base16 "f666f6f62"     = some rfc4       := rfl
theorem decodeBase16Rfc5 : decode Base16 "f666f6f6261"   = some rfc5       := rfl
theorem decodeBase16Rfc6 : decode Base16 "f666f6f626172" = some rfc6       := rfl

theorem encodeBase16UpperRfc0 : encode Base16Upper rfc0 = "F"              := rfl
theorem encodeBase16UpperRfc1 : encode Base16Upper rfc1 = "F66"            := rfl
theorem encodeBase16UpperRfc2 : encode Base16Upper rfc2 = "F666F"          := rfl
theorem encodeBase16UpperRfc3 : encode Base16Upper rfc3 = "F666F6F"        := rfl
theorem encodeBase16UpperRfc4 : encode Base16Upper rfc4 = "F666F6F62"      := rfl
theorem encodeBase16UpperRfc5 : encode Base16Upper rfc5 = "F666F6F6261"    := rfl
theorem encodeBase16UpperRfc6 : encode Base16Upper rfc6 = "F666F6F626172"  := rfl

theorem decodeBase16UpperRfc0 : decode Base16Upper "F"             = some rfc0 := rfl
theorem decodeBase16UpperRfc1 : decode Base16Upper "F66"           = some rfc1 := rfl
theorem decodeBase16UpperRfc2 : decode Base16Upper "F666F"         = some rfc2 := rfl
theorem decodeBase16UpperRfc3 : decode Base16Upper "F666F6F"       = some rfc3 := rfl
theorem decodeBase16UpperRfc4 : decode Base16Upper "F666F6F62"     = some rfc4 := rfl
theorem decodeBase16UpperRfc5 : decode Base16Upper "F666F6F6261"   = some rfc5 := rfl
theorem decodeBase16UpperRfc6 : decode Base16Upper "F666F6F626172" = some rfc6 := rfl

theorem encodeBase32HexRfc0 : encode Base32Hex rfc0 = "v"           := rfl
theorem encodeBase32HexRfc1 : encode Base32Hex rfc1 = "vco"         := rfl
theorem encodeBase32HexRfc2 : encode Base32Hex rfc2 = "vcpng"       := rfl
theorem encodeBase32HexRfc3 : encode Base32Hex rfc3 = "vcpnmu"      := rfl
theorem encodeBase32HexRfc4 : encode Base32Hex rfc4 = "vcpnmuog"    := rfl
theorem encodeBase32HexRfc5 : encode Base32Hex rfc5 = "vcpnmuoj1"   := rfl
theorem encodeBase32HexRfc6 : encode Base32Hex rfc6 = "vcpnmuoj1e8" := rfl

theorem decodeBase32HexRfc0 : decode Base32Hex "v"           = some rfc0 := rfl
theorem decodeBase32HexRfc1 : decode Base32Hex "vco"         = some rfc1 := rfl
theorem decodeBase32HexRfc2 : decode Base32Hex "vcpng"       = some rfc2 := rfl
theorem decodeBase32HexRfc3 : decode Base32Hex "vcpnmu"      = some rfc3 := rfl
theorem decodeBase32HexRfc4 : decode Base32Hex "vcpnmuog"    = some rfc4 := rfl
theorem decodeBase32HexRfc5 : decode Base32Hex "vcpnmuoj1"   = some rfc5 := rfl
theorem decodeBase32HexRfc6 : decode Base32Hex "vcpnmuoj1e8" = some rfc6 := rfl

theorem encodeBase32HexUpperRfc0 : encode Base32HexUpper rfc0 = "V"           := rfl
theorem encodeBase32HexUpperRfc1 : encode Base32HexUpper rfc1 = "VCO"         := rfl
theorem encodeBase32HexUpperRfc2 : encode Base32HexUpper rfc2 = "VCPNG"       := rfl
theorem encodeBase32HexUpperRfc3 : encode Base32HexUpper rfc3 = "VCPNMU"      := rfl
theorem encodeBase32HexUpperRfc4 : encode Base32HexUpper rfc4 = "VCPNMUOG"    := rfl
theorem encodeBase32HexUpperRfc5 : encode Base32HexUpper rfc5 = "VCPNMUOJ1"   := rfl
theorem encodeBase32HexUpperRfc6 : encode Base32HexUpper rfc6 = "VCPNMUOJ1E8" := rfl

theorem decodeBase32HexUpperRfc0 : decode Base32HexUpper "V"           = some rfc0  := rfl
theorem decodeBase32HexUpperRfc1 : decode Base32HexUpper "VCO"         = some rfc1  := rfl
theorem decodeBase32HexUpperRfc2 : decode Base32HexUpper "VCPNG"       = some rfc2  := rfl
theorem decodeBase32HexUpperRfc3 : decode Base32HexUpper "VCPNMU"      = some rfc3  := rfl
theorem decodeBase32HexUpperRfc4 : decode Base32HexUpper "VCPNMUOG"    = some rfc4  := rfl
theorem decodeBase32HexUpperRfc5 : decode Base32HexUpper "VCPNMUOJ1"   = some rfc5  := rfl
theorem decodeBase32HexUpperRfc6 : decode Base32HexUpper "VCPNMUOJ1E8" = some rfc6  := rfl

theorem encodeBase32HexPadRfc0 : encode Base32HexPad rfc0 = "t"                 := rfl
theorem encodeBase32HexPadRfc1 : encode Base32HexPad rfc1 = "tco======"         := rfl
theorem encodeBase32HexPadRfc2 : encode Base32HexPad rfc2 = "tcpng===="         := rfl
theorem encodeBase32HexPadRfc3 : encode Base32HexPad rfc3 = "tcpnmu==="         := rfl
theorem encodeBase32HexPadRfc4 : encode Base32HexPad rfc4 = "tcpnmuog="         := rfl
theorem encodeBase32HexPadRfc5 : encode Base32HexPad rfc5 = "tcpnmuoj1"         := rfl
theorem encodeBase32HexPadRfc6 : encode Base32HexPad rfc6 = "tcpnmuoj1e8======" := rfl

theorem decodeBase32HexPadRfc0 : decode Base32HexPad "t"                 = some rfc0  := rfl
theorem decodeBase32HexPadRfc1 : decode Base32HexPad "tco======"         = some rfc1  := rfl
theorem decodeBase32HexPadRfc2 : decode Base32HexPad "tcpng===="         = some rfc2  := rfl
theorem decodeBase32HexPadRfc3 : decode Base32HexPad "tcpnmu==="         = some rfc3  := rfl
theorem decodeBase32HexPadRfc4 : decode Base32HexPad "tcpnmuog="         = some rfc4  := rfl
theorem decodeBase32HexPadRfc5 : decode Base32HexPad "tcpnmuoj1"         = some rfc5  := rfl
theorem decodeBase32HexPadRfc6 : decode Base32HexPad "tcpnmuoj1e8======" = some rfc6  := rfl

theorem encodeBase32HexPadUpperRfc0 : encode Base32HexPadUpper rfc0 = "T"                 := rfl
theorem encodeBase32HexPadUpperRfc1 : encode Base32HexPadUpper rfc1 = "TCO======"         := rfl
theorem encodeBase32HexPadUpperRfc2 : encode Base32HexPadUpper rfc2 = "TCPNG===="         := rfl
theorem encodeBase32HexPadUpperRfc3 : encode Base32HexPadUpper rfc3 = "TCPNMU==="         := rfl
theorem encodeBase32HexPadUpperRfc4 : encode Base32HexPadUpper rfc4 = "TCPNMUOG="         := rfl
theorem encodeBase32HexPadUpperRfc5 : encode Base32HexPadUpper rfc5 = "TCPNMUOJ1"         := rfl
theorem encodeBase32HexPadUpperRfc6 : encode Base32HexPadUpper rfc6 = "TCPNMUOJ1E8======" := rfl

theorem decodeBase32HexPadUpperRfc0 : decode Base32HexPadUpper "T" = some rfc0                 := rfl
theorem decodeBase32HexPadUpperRfc1 : decode Base32HexPadUpper "TCO======" = some rfc1         := rfl
theorem decodeBase32HexPadUpperRfc2 : decode Base32HexPadUpper "TCPNG====" = some rfc2         := rfl
theorem decodeBase32HexPadUpperRfc3 : decode Base32HexPadUpper "TCPNMU===" = some rfc3         := rfl
theorem decodeBase32HexPadUpperRfc4 : decode Base32HexPadUpper "TCPNMUOG=" = some rfc4         := rfl
theorem decodeBase32HexPadUpperRfc5 : decode Base32HexPadUpper "TCPNMUOJ1" = some rfc5         := rfl
theorem decodeBase32HexPadUpperRfc6 : decode Base32HexPadUpper "TCPNMUOJ1E8======" = some rfc6 := rfl

theorem encodeBase32Rfc0 : encode Base32 rfc0 = "b"           := rfl
theorem encodeBase32Rfc1 : encode Base32 rfc1 = "bmy"         := rfl
theorem encodeBase32Rfc2 : encode Base32 rfc2 = "bmzxq"       := rfl
theorem encodeBase32Rfc3 : encode Base32 rfc3 = "bmzxw6"      := rfl
theorem encodeBase32Rfc4 : encode Base32 rfc4 = "bmzxw6yq"    := rfl
theorem encodeBase32Rfc5 : encode Base32 rfc5 = "bmzxw6ytb"   := rfl
theorem encodeBase32Rfc6 : encode Base32 rfc6 = "bmzxw6ytboi" := rfl

theorem decodeBase32Rfc0 : decode Base32 "b"            = some rfc0 := rfl
theorem decodeBase32Rfc1 : decode Base32 "bmy"          = some rfc1 := rfl
theorem decodeBase32Rfc2 : decode Base32 "bmzxq"        = some rfc2 := rfl
theorem decodeBase32Rfc3 : decode Base32 "bmzxw6"       = some rfc3 := rfl
theorem decodeBase32Rfc4 : decode Base32 "bmzxw6yq"     = some rfc4 := rfl
theorem decodeBase32Rfc5 : decode Base32 "bmzxw6ytb"    = some rfc5 := rfl
theorem decodeBase32Rfc6 : decode Base32 "bmzxw6ytboi"  = some rfc6 := rfl

theorem encodeBase32UpperRfc0 : encode Base32Upper rfc0 = "B"           := rfl
theorem encodeBase32UpperRfc1 : encode Base32Upper rfc1 = "BMY"         := rfl
theorem encodeBase32UpperRfc2 : encode Base32Upper rfc2 = "BMZXQ"       := rfl
theorem encodeBase32UpperRfc3 : encode Base32Upper rfc3 = "BMZXW6"      := rfl
theorem encodeBase32UpperRfc4 : encode Base32Upper rfc4 = "BMZXW6YQ"    := rfl
theorem encodeBase32UpperRfc5 : encode Base32Upper rfc5 = "BMZXW6YTB"   := rfl
theorem encodeBase32UpperRfc6 : encode Base32Upper rfc6 = "BMZXW6YTBOI" := rfl

theorem encodeBase32PadRfc0 : encode Base32Pad rfc0 = "c"                 := rfl
theorem encodeBase32PadRfc1 : encode Base32Pad rfc1 = "cmy======"         := rfl
theorem encodeBase32PadRfc2 : encode Base32Pad rfc2 = "cmzxq===="         := rfl
theorem encodeBase32PadRfc3 : encode Base32Pad rfc3 = "cmzxw6==="         := rfl
theorem encodeBase32PadRfc4 : encode Base32Pad rfc4 = "cmzxw6yq="         := rfl
theorem encodeBase32PadRfc5 : encode Base32Pad rfc5 = "cmzxw6ytb"         := rfl
theorem encodeBase32PadRfc6 : encode Base32Pad rfc6 = "cmzxw6ytboi======" := rfl

theorem encodeBase32PadUpperRfc0 : encode Base32PadUpper rfc0 = "C"                 := rfl
theorem encodeBase32PadUpperRfc1 : encode Base32PadUpper rfc1 = "CMY======"         := rfl
theorem encodeBase32PadUpperRfc2 : encode Base32PadUpper rfc2 = "CMZXQ===="         := rfl
theorem encodeBase32PadUpperRfc3 : encode Base32PadUpper rfc3 = "CMZXW6==="         := rfl
theorem encodeBase32PadUpperRfc4 : encode Base32PadUpper rfc4 = "CMZXW6YQ="         := rfl
theorem encodeBase32PadUpperRfc5 : encode Base32PadUpper rfc5 = "CMZXW6YTB"         := rfl
theorem encodeBase32PadUpperRfc6 : encode Base32PadUpper rfc6 = "CMZXW6YTBOI======" := rfl

theorem encodeBase64Rfc0 : encode Base64 rfc0 = "m"         := rfl
theorem encodeBase64Rfc1 : encode Base64 rfc1 = "mZg"       := rfl
theorem encodeBase64Rfc2 : encode Base64 rfc2 = "mZm8"      := rfl
theorem encodeBase64Rfc3 : encode Base64 rfc3 = "mZm9v"     := rfl
theorem encodeBase64Rfc4 : encode Base64 rfc4 = "mZm9vYg"   := rfl
theorem encodeBase64Rfc5 : encode Base64 rfc5 = "mZm9vYmE"  := rfl
theorem encodeBase64Rfc6 : encode Base64 rfc6 = "mZm9vYmFy" := rfl

theorem decodeBase64Rfc0 : decode Base64 "m"          = some rfc0 := rfl
theorem decodeBase64Rfc1 : decode Base64 "mZg"        = some rfc1 := rfl
theorem decodeBase64Rfc2 : decode Base64 "mZm8"       = some rfc2 := rfl
theorem decodeBase64Rfc3 : decode Base64 "mZm9v"      = some rfc3 := rfl
theorem decodeBase64Rfc4 : decode Base64 "mZm9vYg"    = some rfc4 := rfl
theorem decodeBase64Rfc5 : decode Base64 "mZm9vYmE"   = some rfc5 := rfl
theorem decodeBase64Rfc6 : decode Base64 "mZm9vYmFy"  = some rfc6 := rfl

theorem encodeBase64PadRfc0 : encode Base64Pad rfc0 = "M"         := rfl
theorem encodeBase64PadRfc1 : encode Base64Pad rfc1 = "MZg=="     := rfl
theorem encodeBase64PadRfc2 : encode Base64Pad rfc2 = "MZm8="     := rfl
theorem encodeBase64PadRfc3 : encode Base64Pad rfc3 = "MZm9v"     := rfl
theorem encodeBase64PadRfc4 : encode Base64Pad rfc4 = "MZm9vYg==" := rfl
theorem encodeBase64PadRfc5 : encode Base64Pad rfc5 = "MZm9vYmE=" := rfl
theorem encodeBase64PadRfc6 : encode Base64Pad rfc6 = "MZm9vYmFy" := rfl

theorem encodeBase64URLPadRfc0 : encode Base64URLPad rfc0 = "U"         := rfl
theorem encodeBase64URLPadRfc1 : encode Base64URLPad rfc1 = "UZg=="     := rfl
theorem encodeBase64URLPadRfc2 : encode Base64URLPad rfc2 = "UZm8="     := rfl
theorem encodeBase64URLPadRfc3 : encode Base64URLPad rfc3 = "UZm9v"     := rfl
theorem encodeBase64URLPadRfc4 : encode Base64URLPad rfc4 = "UZm9vYg==" := rfl
theorem encodeBase64URLPadRfc5 : encode Base64URLPad rfc5 = "UZm9vYmE=" := rfl
theorem encodeBase64URLPadRfc6 : encode Base64URLPad rfc6 = "UZm9vYmFy" := rfl

end RFC4648

end Multibase.Test
