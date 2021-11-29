import Ipld.Multibase.Class

inductive Base
| base2
| base8
| base10
| base16
| base16upper
| base32hex
| base32hexupper
| base32hexpad
| base32hexpadupper
| base32
| base32upper
| base32pad
| base32padupper
| base32z
| base36
| base36upper
| base58btc
| base58flickr
| base64
| base64pad
| base64url
| base64urlpad


def encode (b: Base) (x: ByteArray) : String :=
  match b with
  | Base.base2            => Multibase.encode Base2 x.data.data
  | Base.base8            => Multibase.encode Base8 x.data.data
  | Base.base10           => Multibase.encode Base10 x.data.data
  | Base.base16           => Multibase.encode Base16 x.data.data
  | Base.base16upper      => Multibase.encode Base16Upper x.data.data
  | Base.base32hex        => Multibase.encode Base32Hex x.data.data
  | Base.base32hexupper   => Multibase.encode Base32HexUpper x.data.data
  | Base.base32hexpad     => Multibase.encode Base32HexPad x.data.data
  | Base.base32hexpadupper=> Multibase.encode Base32HexPadUpper x.data.data
  | Base.base32           => Multibase.encode Base32 x.data.data
  | Base.base32upper      => Multibase.encode Base32Upper x.data.data
  | Base.base32pad        => Multibase.encode Base32Pad x.data.data
  | Base.base32padupper   => Multibase.encode Base32PadUpper x.data.data
  | Base.base32z          => Multibase.encode Base32Z x.data.data
  | Base.base36           => Multibase.encode Base36 x.data.data
  | Base.base36upper      => Multibase.encode Base36Upper x.data.data
  | Base.base58btc        => Multibase.encode Base58BTC x.data.data
  | Base.base58flickr     => Multibase.encode Base58Flickr x.data.data
  | Base.base64           => Multibase.encode Base64 x.data.data
  | Base.base64pad        => Multibase.encode Base64Pad x.data.data
  | Base.base64url        => Multibase.encode Base64URL x.data.data
  | Base.base64urlpad     => Multibase.encode Base64URLPad x.data.data

def decode (b: Base) (x: String) : Option ByteArray :=
  match b with
  | Base.base2            => ByteArray.mk <$> Array.mk <$> Multibase.decode Base2 x
  | Base.base8            => ByteArray.mk <$> Array.mk <$> Multibase.decode Base8 x
  | Base.base10           => ByteArray.mk <$> Array.mk <$> Multibase.decode Base10 x
  | Base.base16           => ByteArray.mk <$> Array.mk <$> Multibase.decode Base16 x
  | Base.base16upper      => ByteArray.mk <$> Array.mk <$> Multibase.decode Base16Upper x
  | Base.base32hex        => ByteArray.mk <$> Array.mk <$> Multibase.decode Base32Hex x
  | Base.base32hexupper   => ByteArray.mk <$> Array.mk <$> Multibase.decode Base32HexUpper x
  | Base.base32hexpad     => ByteArray.mk <$> Array.mk <$> Multibase.decode Base32HexPad x
  | Base.base32hexpadupper=> ByteArray.mk <$> Array.mk <$> Multibase.decode Base32HexPadUpper x
  | Base.base32           => ByteArray.mk <$> Array.mk <$> Multibase.decode Base32 x
  | Base.base32upper      => ByteArray.mk <$> Array.mk <$> Multibase.decode Base32Upper x
  | Base.base32pad        => ByteArray.mk <$> Array.mk <$> Multibase.decode Base32Pad x
  | Base.base32padupper   => ByteArray.mk <$> Array.mk <$> Multibase.decode Base32PadUpper x
  | Base.base32z          => ByteArray.mk <$> Array.mk <$> Multibase.decode Base32Z x
  | Base.base36           => ByteArray.mk <$> Array.mk <$> Multibase.decode Base36 x
  | Base.base36upper      => ByteArray.mk <$> Array.mk <$> Multibase.decode Base36Upper x
  | Base.base58btc        => ByteArray.mk <$> Array.mk <$> Multibase.decode Base58BTC x
  | Base.base58flickr     => ByteArray.mk <$> Array.mk <$> Multibase.decode Base58Flickr x
  | Base.base64           => ByteArray.mk <$> Array.mk <$> Multibase.decode Base64 x
  | Base.base64pad        => ByteArray.mk <$> Array.mk <$> Multibase.decode Base64Pad x
  | Base.base64url        => ByteArray.mk <$> Array.mk <$> Multibase.decode Base64URL x
  | Base.base64urlpad     => ByteArray.mk <$> Array.mk <$> Multibase.decode Base64URLPad x

