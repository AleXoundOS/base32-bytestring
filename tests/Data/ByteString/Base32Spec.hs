{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}
module Data.ByteString.Base32Spec (spec) where

import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import Data.ByteString.Base32 as Base32
import Data.Char
import Test.Hspec
import Test.QuickCheck


instance Arbitrary ByteString where
  arbitrary = BS.pack <$> arbitrary

spec :: Spec
spec = do
  describe "encode" $ do
    it "conform RFC examples" $ do
      encode ""       `shouldBe` ""
      encode "f"      `shouldBe` "MY======"
      encode "fo"     `shouldBe` "MZXQ===="
      encode "foo"    `shouldBe` "MZXW6==="
      encode "foob"   `shouldBe` "MZXW6YQ="
      encode "fooba"  `shouldBe` "MZXW6YTB"
      encode "foobar" `shouldBe` "MZXW6YTBOI======"

    it "size always multiple of 8 bytes" $ property $ \bs ->
      (BS.length (encode bs) `rem` 8) `shouldBe` 0

    it "padding less than 8 bytes" $ property $ \bs ->
      BC.count '=' bs `shouldSatisfy` (< 8)

  describe "decode" $ do
    it "conform RFC examples" $ do
      decode ""                 `shouldBe` Right ""
      decode "MY======"         `shouldBe` Right "f"
      decode "MZXQ===="         `shouldBe` Right "fo"
      decode  "MZXW6==="        `shouldBe` Right "foo"
      decode "MZXW6YQ="         `shouldBe` Right "foob"
      decode "MZXW6YTB"         `shouldBe` Right "fooba"
      decode "MZXW6YTBOI======" `shouldBe` Right "foobar"

    it "inverse for encode" $ property $ \bs ->
      decode (encode bs) == Right bs

    it "case insensitive" $ property $ \bs ->
      decode (BC.map toLower (encode bs)) == Right bs

    it "fail gracefully if encoded data contains non alphabet chars" $ do
      decode "0======="         `shouldBe` Left "'0' is not base32 character"
      decode "AAAAAAAA0=======" `shouldBe` Left "'0' is not base32 character"

  describe "decodeLenient" $ do
    it "conform RFC examples" $ do
      decodeLenient ""                 `shouldBe` Right ""
      decodeLenient "MY======"         `shouldBe` Right "f"
      decodeLenient "MZXQ===="         `shouldBe` Right "fo"
      decodeLenient  "MZXW6==="        `shouldBe` Right "foo"
      decodeLenient "MZXW6YQ="         `shouldBe` Right "foob"
      decodeLenient "MZXW6YTB"         `shouldBe` Right "fooba"
      decodeLenient "MZXW6YTBOI======" `shouldBe` Right "foobar"

    it "inverse for encode" $ property $ \bs ->
      decodeLenient (encode bs) == Right bs

    it "case insensitive" $ property $ \bs ->
      decodeLenient (BC.map toLower (encode bs)) == Right bs

    it "skip non alphabet chars" $ do
      decodeLenient "|"   `shouldBe` Right ""
      decodeLenient "M|Y" `shouldBe` Right "f"
