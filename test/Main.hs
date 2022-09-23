{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Main where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.ByteString
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Arbitrary
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import System.Timeout
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.ShouldNotTypecheck (shouldNotTypecheck)

import Data.Text.Display

main :: IO ()
main = hspec spec

data AutomaticallyDerived = AD
  deriving stock (Show)
  deriving (Display) via (ShowInstance AutomaticallyDerived)

data ManualType = MT Int

instance Display ManualType where
  displayPrec prec (MT i) = displayParen (prec > 10) $ "MT " <> displayPrec 11 i

data OpaqueType = OpaqueType Int
  deriving
    (Display)
    via (OpaqueInstance "<opaque>" OpaqueType)

-- | @v \`shouldEvaluateWithin\` n@ sets the expectation that evaluating @v@
-- should take no longer than @n@ microseconds.
shouldEvaluateWithin :: (HasCallStack, NFData a) => a -> Int -> Expectation
shouldEvaluateWithin a n = do
  res <- timeout n (evaluate $ force a)
  when (isNothing res) $ do
    expectationFailure ("evaluation timed out in " <> show n <> " microseconds")

spec :: Spec
spec = do
  describe "Display Tests:" $ do
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
    it "List instance is streamed lazily" $ do
      let list = [1 ..] :: [Int]
      TL.take 20 (TB.toLazyText $ displayBuilder list) `shouldEvaluateWithin` 100000
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
    it "Char '\'' instance is equivalent to Text" $ do
      display '\'' `shouldBe` T.singleton '\''
    it "2-Tuple instance is equivalent to Show" $ do
      let tuple = (1 :: Int, True)
      T.unpack (display tuple) `shouldBe` show tuple
    it "3-Tuple instance is equivalent to Show" $ do
      let tuple = (1 :: Int, True, "hahahha" :: String)
      display tuple `shouldBe` "(" <> display (1 :: Int) <> "," <> display True <> "," <> display @String "hahahha" <> ")"

  describe "Display props:" $ do
    prop "Text instance stays the same" $ do
      \string -> display (string :: Text) `shouldBe` string
    prop "String instance is equivalent to Text" $ do
      \string -> display (string :: String) `shouldBe` T.pack string
    prop "Chars are packed" $
      \c -> display (c :: Char) `shouldBe` T.singleton c

  describe "Forbidden instances" $ do
    it "Should not compile for a function instance" $
      shouldNotTypecheck (display id) `shouldThrow` anyErrorCall
    it "Should not compile for ByteStrings" $
      let bs = "badstring" :: ByteString
       in shouldNotTypecheck (display bs) `shouldThrow` anyErrorCall

  describe "displayParen tests" $ do
    it "Surrounds with parens when True" $
      displayParen True "foo" `shouldBe` "(foo)"
    it "Doesn't surround with parens when False" $
      displayParen False "foo" `shouldBe` "foo"
    it "Surrounds deeply-nested Maybes with a prec of 10" $
      displayPrec 10 (Just (Just (Just (3 :: Int)))) `shouldBe` "Just (Just (Just 3))"
    it "Surrounds deeply-nested Maybes with a prec of 11" $
      displayPrec 11 (Just (Just (Just (3 :: Int)))) `shouldBe` "(Just (Just (Just 3)))"
