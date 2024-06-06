{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Arbitrary
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Data.Text.Builder.Linear as Builder
import Data.Text.Display

main :: IO ()
main = defaultMain spec

shouldThrow :: (HasCallStack, Exception e, Eq e) => IO a -> e -> Assertion
shouldThrow action expectedException = do
  result <- try action
  case result of
    Right _ ->
      assertFailure $ "Expected test to fail with " <> show expectedException
    Left actualException ->
      assertEqual
        ("Expected test to fail with" <> show expectedException <> " but failed with " <> show actualException <> " instead.")
        expectedException
        actualException

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
shouldEvaluateWithin :: (HasCallStack, NFData a) => a -> Int -> Assertion
shouldEvaluateWithin a n = do
  res <- timeout n (evaluate $ force a)
  when (isNothing res) $ do
    assertFailure ("evaluation timed out in " <> show n <> " microseconds")

spec :: TestTree
spec =
  testGroup
    "Display Tests"
    [ testGroup
        "Instances Tests"
        [ testCase "Display instance for Text stays the same" $
            display ("3" :: Text) @?= ("3" :: Text)
        , testCase "Deriving via its own Show instance works" $
            T.unpack (display AD) @?= show AD
        , testCase "Opaque types stay opaque" $
            display (OpaqueType 3 :: OpaqueType) @?= "<opaque>"
        , testCase "Manual instance is stays the same" $
            display (MT 32) @?= "MT 32"
        , testCase "List instance is equivalent to Show" $ do
            let list = [1 .. 5] :: [Int]
            T.unpack (display list) @?= show list
        , testCase "Single-element List instance is equivalent to Show" $ do
            let list = [1] :: [Int]
            T.unpack (display list) @?= show list
        , testCase "List instance is streamed lazily" $ do
            let list = [1 ..] :: [Int]
            T.take 20 (Builder.runBuilder $ displayBuilder list) `shouldEvaluateWithin` 100000
        , testCase "NonEmpty instance is equivalent to Show" $ do
            let ne = NE.fromList [1 .. 5] :: NonEmpty Int
            T.unpack (display ne) @?= show ne
        , testCase "Just True instance is equivalent to Show" $ do
            T.unpack (display (Just True)) @?= show (Just True)
        , testCase "Nested Maybe instance is equivalent to Show" $ do
            let nestedMaybe = Just (Just 5) :: Maybe (Maybe Int)
            T.unpack (display nestedMaybe) @?= show nestedMaybe
        , testCase "Nothing instance is equivalent to Show" $ do
            T.unpack (display (Nothing @Bool)) @?= show (Nothing @Bool)
        , testCase "Char '\'' instance is equivalent to Text" $ do
            display '\'' @?= T.singleton '\''
        , testCase "2-Tuple instance is equivalent to Show" $ do
            let tuple = (1 :: Int, True)
            T.unpack (display tuple) @?= show tuple
        , testCase "3-Tuple instance is equivalent to Show" $ do
            let tuple = (1 :: Int, True, "hahahha" :: String)
            display tuple @?= "(" <> display (1 :: Int) <> "," <> display True <> "," <> display @String "hahahha" <> ")"
        ]
    , testGroup
        "Properties Tests"
        [ testProperty "Text instance stays the same" $ do
            \string -> display (string :: Text) === string
        , testProperty "String instance is equivalent to Text" $ do
            \string -> display (string :: String) === T.pack string
        , testProperty "Chars are packed" $
            \c -> display (c :: Char) === T.singleton c
        ]
    , testGroup
        "`displayParen` tests"
        [ testCase "Surrounds with parens when True" $
            Builder.runBuilder (displayParen True "foo") @?= "(foo)"
        , testCase "Doesn't surround with parens when False" $
            Builder.runBuilder (displayParen False "foo") @?= "foo"
        , testCase "Surrounds deeply-nested Maybes with a prec of 10" $
            Builder.runBuilder (displayPrec 10 (Just (Just (Just (3 :: Int))))) @?= "Just (Just (Just 3))"
        , testCase "Surrounds deeply-nested Maybes with a prec of 11" $
            Builder.runBuilder (displayPrec 11 (Just (Just (Just (3 :: Int))))) @?= "(Just (Just (Just 3)))"
        ]
    ]
