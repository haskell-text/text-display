{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-defer-type-errors #-}

module Main where

import Data.ByteString
import Data.List.NonEmpty
import Data.Text (Text)
import Test.Hspec
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import Data.Text.Display

main :: IO ()
main = hspec spec

data AutomaticallyDerived = AD
  deriving stock Show
  deriving Display via (ShowInstance AutomaticallyDerived)

data ManualType = MT Int

instance Display ManualType where
  displayBuilder (MT i) = "MT " <> displayBuilder i

data OpaqueType = OpaqueType Int
  deriving Display
    via (OpaqueInstance "<opaque>" OpaqueType)

spec :: Spec
spec = do
  describe "Display Tests" $ do
    it "Display instance for Text stays the same" $
      display ("3" :: Text) `shouldBe` ("3" :: Text)
    it "Deriving via its own Show instance works" $
      T.unpack (display AD) `shouldBe` show AD
    it "Opaque types stay opaque" $
      display (OpaqueType 3 :: OpaqueType) `shouldBe` "<opaque>"
    it "Manual instance is stays the same" $
      display (MT 32) `shouldBe` "MT 32"
    it "List instance is equivalent to Show" $ do
      let list = [1 .. 5] :: [Int]
      T.unpack (display list) `shouldBe` show list
    it "Single-element List instance is equivalent to Show" $ do
      let list = [1] :: [Int]
      T.unpack (display list) `shouldBe` show list
    it "NonEmpty instance is equivalent to Show" $ do
      let ne = NE.fromList [1 .. 5] :: NonEmpty Int
      T.unpack (display ne) `shouldBe` show ne
    it "Just True instance is equivalent to Show" $ do
      T.unpack (display (Just True)) `shouldBe` show (Just True)
    it "Nested Maybe instance is equivalent to Show" $ do
      let nestedMaybe = Just (Just 5) :: Maybe (Maybe Int)
      T.unpack (display nestedMaybe) `shouldBe` show nestedMaybe
    it "Nothing instance is equivalent to Show" $ do
      T.unpack (display (Nothing @Bool)) `shouldBe` show (Nothing @Bool)
    it "String instance is equivalent to Show" $ do
      let string = "Bonjour \"tout le monde\" !" :: String
      T.unpack (display string) `shouldBe` show string
    it "Char 'c' instance is equivalent to Show" $ do
      T.unpack (display 'c') `shouldBe` show 'c'
    it "Char '\'' instance is equivalent to Show" $ do
      T.unpack (display '\'') `shouldBe` show '\''
    it "2-Tuple instance is equivalent to Show" $ do
      let tuple = (1 :: Int, True)
      T.unpack (display tuple) `shouldBe` show tuple
    it "3-Tuple instance is equivalent to Show" $ do
      let tuple = (1 :: Int, True, "hahahha" :: String)
      T.unpack (display tuple) `shouldBe` show tuple

    it "Should not compile for a function instance" $
      shouldNotTypecheck (display id) `shouldThrow` anyErrorCall
    it "Should not compile for ByteStrings" $
      let bs = "badstring" :: ByteString
       in shouldNotTypecheck (display bs) `shouldThrow` anyErrorCall

  describe "displayParen tests" $ do
    it "surrounds with parens when True" $
      displayParen True "foo" `shouldBe` "(foo)"
    it "doesn't surround with parens when False" $
      displayParen False "foo" `shouldBe` "foo"
