
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Data.Text.Display

main :: IO ()
main = hspec spec

data AutomaticallyDerived = AD deriving stock (Show)
  deriving (Display)
    via (ShowInstance AutomaticallyDerived)

data ManualType
  = MT Int

instance Display ManualType where
  display (MT i) = "MT " <> display i

newtype Opaque a
  = Opaque a

instance Display (Opaque a) where
  display = const "<opaque>"

spec :: Spec
spec = do
    describe "Display Tests" $ do
      it "Display instance for Text stays the same" $
        display ("3" :: Text) `shouldBe` ("3" :: Text)
      it "Deriving via its own Show instance works" $
        show AD `shouldBe`  T.unpack (display AD)
      it "Opaque types stay opaque" $
        display (Opaque 3 :: Opaque Int) `shouldBe` "<opaque>"
      it "Manual instance is stays the same" $
        display (MT 32) `shouldBe` "MT 32"
      it "List instance is equivalent to Show" $ do
        let list = [1..5] :: [Int]
        show list `shouldBe` T.unpack (display list)
      it "NonEmpty instance is equivalent to Show" $ do
        let ne = NE.fromList [1..5] :: NonEmpty Int
        show ne `shouldBe` T.unpack (display ne)
      it "Maybe instance is equivalent to Show" $ do
        show (Just True) `shouldBe` T.unpack (display (Just True))
        show (Nothing @Bool) `shouldBe` T.unpack (display (Nothing @Bool))
      it "String instance is equivalent to Show" $ do
        let string = "Bonjour tout le monde !" :: String
        show string `shouldBe` T.unpack (display string)
      it "2-Tuple instance is equivalent to Show" $ do
        let tuple = (1 :: Int, True)
        show tuple `shouldBe` T.unpack (display tuple)
      it "3-Tuple instance is equivalent to Show" $ do
        let tuple = (1 :: Int, True, "hahahha" :: String)
        show tuple `shouldBe` T.unpack (display tuple)

      -- This test must never compile:
      -- it "Function instance" $
      --   display (+)
