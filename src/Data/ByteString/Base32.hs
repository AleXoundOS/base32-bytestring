-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
--   Efficient encoding and decoding of base32 encoded bytestring
--   according to RFC 4648. <http://tools.ietf.org/html/rfc4648>
--
--   This module recommended to be imported as
--   @import Data.ByteString.Base32 as Base32@ to avoid name clashes
--   with @Data.Binary@ or @Data.ByteString.Base64@ modules.
--
{-# LANGUAGE BangPatterns #-}
module Data.ByteString.Base32
       ( Base32
       , encode
       , decode
       , decodeLenient
       ) where

import Data.ByteString as BS
import Data.ByteString.Base32.Internal
import Data.List as L


-- | Base32 encoded bytestring.
type Base32 = ByteString

encW5 :: Word5 -> Word8
encW5 !x
  |  x <= 25  = 65 + x
  | otherwise = 24 + x
{-# INLINE encW5 #-}

encTable :: EncTable
encTable = BS.pack $ L.map encW5 [0..31]

-- | Encode an arbitrary bytestring into (upper case) base32 form.
encode :: ByteString -> Base32
encode = unpack5 encTable

decW5 :: Word8 -> Word5
decW5 !x
  | x <  48  {- c2w '0' -} = invIx
  | x <= 57  {- c2w '9' -} = x
  | x <  97  {- c2w 'a' -} = invIx
  | x <= 122 {- c2w 'z' -} = x - (97 - 48) + 1
  | otherwise = invIx
{-# INLINE decW5 #-}

decTable :: ByteString
decTable = BS.pack $ L.map decW5 [minBound .. maxBound]

-- | Decode a base32 encoded bytestring. This functions is
-- case-insensitive and do not require correct padding.
decode :: Base32 -> Either String ByteString
decode = pack5 decTable

-- | The same as 'decode' but with additional leniency: decodeLenient
-- will skip non-alphabet characters.
decodeLenient :: Base32 -> Either String ByteString
decodeLenient = pack5Lenient decTable
