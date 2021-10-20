{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
  Module      : Data.Text.Entity
  Copyright   : © Hécate Moonlight, 2021
  License     : MIT
  Maintainer  : hecate@glitchbra.in
  Stability   : stable

  The Display typeclass provides a solution for user-facing output that does not have to abide by the rules of the Show typeclass.

-}
module Data.Text.Display
  ( -- * Documentation
    Display(..)
  , ShowInstance(..)
  -- * Design choices
  -- $designChoices
  ) where

import Control.Exception hiding (TypeError)
import Data.ByteString
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Kind
import Data.List.NonEmpty
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word
import GHC.TypeLits
import Data.Foldable (foldMap')

-- | A typeclass for user-facing output.
--
-- @since 0.0.1.0
class Display a where
  {-# MINIMAL display #-}
  -- | Convert a value to a readable 'Text'.
  --
  -- === Examples
  -- >>> display 3
  -- "3"
  --
  -- >>> display True
  -- "True"
  --
  display :: a -> Text

  -- | The method 'displayList' is provided to allow for a specialised
  -- way to render lists of a certain value.
  -- This is used to render the list of 'Char' as a string of characters
  -- enclosed in double quotes, rather than between square brackets and
  -- separated by commas.
  --
  -- === Example
  --
  -- > instance Display Char where
  -- >   display = T.singleton
  -- >   -- 'displayList' is implemented, so that when the `Display [a]` instance calls 'displayList',
  -- >   -- we end up with a nice string enclosed between double quotes.
  -- >   displayList cs = "\"" <> foldMap' display cs <> "\""
  --
  -- > instance Display a => Display [a] where
  -- > -- In this instance, 'display' is defined in terms of 'displayList', which for most types
  -- > -- is defined as the default written in the class declaration.
  -- > -- But when a ~ Char, there is an explicit implementation that is selected instead, which
  -- > -- provides the rendering of the character string between double quotes.
  -- >   display = displayList
  --
  -- ==== How implementations are selected
  -- >
  -- >                                                              Yes: Custom `displayList` (as seen above)
  -- >                                                             🡕
  -- > '[a]' (List) instance → `display = displayList` →  a ~ Char ?
  -- >                                                             🡖
  -- >                                                              No: Default `displayList`
  displayList :: [a] -> Text
  displayList [] = "[]"
  displayList (x:xs) = displayList' xs ("[" <> display x)
    where
      displayList' :: Display a => [a] -> Text -> Text
      displayList' [] acc = acc <> "]"
      displayList' (y:ys) acc = displayList' ys (acc <> "," <> display y)

-- | 🚫 You should not derive Display for function types!
--
-- 💡 Write a 'newtype' wrapper that represents your domain more accurately.
--    If you are not consciously trying to use `display` on a function,
--    make sure that you are not missing an argument somewhere.
--
-- @since 0.0.1.0
instance CannotDisplayBareFunctions => Display (a -> b) where
  display = undefined

-- | @since 0.0.1.0
type family CannotDisplayBareFunctions :: Constraint where
  CannotDisplayBareFunctions = TypeError
    ( 'Text "🚫 You should not derive Display for function types!" ':$$:
      'Text "💡 Write a 'newtype' wrapper that represents your domain more accurately." ':$$:
      'Text "   If you are not consciously trying to use `display` on a function," ':$$:
      'Text "   make sure that you are not missing an argument somewhere."
    )

-- | 🚫 You should not derive Display for strict ByteStrings!
--
-- 💡 Always provide an explicit encoding.
-- Use 'decodeUtf8' or 'decodeUtf8Strict' to convert from UTF-8
--
-- @since 0.0.1.0
instance CannotDisplayByteStrings => Display ByteString where
  display = undefined

-- | 🚫 You should not derive Display for lazy ByteStrings!
--
-- 💡 Always provide an explicit encoding.
-- Use 'decodeUtf8' or 'decodeUtf8Strict' to convert from UTF-8
--
-- @since 0.0.1.0
instance CannotDisplayByteStrings => Display BL.ByteString where
  display = undefined

type family CannotDisplayByteStrings :: Constraint where
  CannotDisplayByteStrings = TypeError
    ( 'Text "🚫 You should not derive Display for ByteStrings!" ':$$:
      'Text "💡 Always provide an explicit encoding" ':$$:
      'Text     "Use 'decodeUtf8' or 'decodeUtf8Strict' to convert from UTF-8"
    )

-- | This wrapper allows you to rely on a pre-existing 'Show' instance in order to
-- derive 'Display' from it.
--
-- === Example
--
-- > data AutomaticallyDerived = AD
-- >  -- We derive 'Show'
-- >  deriving stock Show
-- >  -- We take advantage of the 'Show' instance to derive 'Display' from it
-- >  deriving Display
-- >    via (ShowInstance AutomaticallyDerived)
--
-- @since 0.0.1.0
newtype (Show e) => ShowInstance e
  = ShowInstance e
  deriving newtype ( Show -- ^ @since 0.0.1.0
                   )

-- | This wrapper allows you to rely on a pre-existing 'Show' instance in order to derive 'Display' from it.
--
-- @since 0.0.1.0
instance Show e => Display (ShowInstance e) where
  display s = T.pack $ show s

-- | @since 0.0.1.0
deriving via (ShowInstance ()) instance Display ()

-- | @since 0.0.1.0
deriving via (ShowInstance Bool) instance Display Bool

-- | @since 0.0.1.0
instance Display Char where
  display = T.singleton
  -- 'displayList' is overloaded, so that when the @Display [a]@ instance calls 'displayList',
  -- we end up with a nice string enclosed between double quotes.
  displayList cs = "\"" <> foldMap' display cs <> "\""

-- | Lazy 'TL.Text'
--
-- @since 0.0.1.0
instance Display TL.Text where
  display = TL.toStrict

-- | Strict 'Data.Text.Text'
--
-- @since 0.0.1.0
instance Display Text where
  display = id

-- | @since 0.0.1.0
instance Display a => Display [a] where
  -- In this instance, 'display' is defined in terms of 'displayList', which for most types
  -- is defined as the default written in the class declaration.
  -- But when @a ~ Char@, there is an explicit implementation that is selected instead, which
  -- provides the rendering of the character string between double quotes.
  display = displayList

-- | @since 0.0.1.0
instance Display a => Display (NonEmpty a) where
  display (a :| as) = display a <> T.pack " :| " <> display as

-- | @since 0.0.1.0
instance Display a => Display (Maybe a) where
  display Nothing = T.pack "Nothing"
  display (Just a) = T.pack "Just " <> display a

-- | @since 0.0.1.0
deriving via (ShowInstance Double) instance Display Double

-- | @since 0.0.1.0
deriving via (ShowInstance Float) instance Display Float

-- | @since 0.0.1.0
deriving via (ShowInstance Int) instance Display Int

-- | @since 0.0.1.0
deriving via (ShowInstance Int8) instance Display Int8

-- | @since 0.0.1.0
deriving via (ShowInstance Int16) instance Display Int16

-- | @since 0.0.1.0
deriving via (ShowInstance Int32) instance Display Int32

-- | @since 0.0.1.0
deriving via (ShowInstance Int64) instance Display Int64

-- | @since 0.0.1.0
deriving via (ShowInstance Integer) instance Display Integer

-- | @since 0.0.1.0
deriving via (ShowInstance Word) instance Display Word

-- | @since 0.0.1.0
deriving via (ShowInstance Word8) instance Display Word8

-- | @since 0.0.1.0
deriving via (ShowInstance Word16) instance Display Word16

-- | @since 0.0.1.0
deriving via (ShowInstance Word32) instance Display Word32

-- | @since 0.0.1.0
deriving via (ShowInstance Word64) instance Display Word64

-- | @since 0.0.1.0
deriving via (ShowInstance IOException) instance Display IOException

-- | @since 0.0.1.0
deriving via (ShowInstance SomeException) instance Display SomeException

-- | @since 0.0.1.0
instance (Display a, Display b) => Display (a, b) where
  display (a, b) = "(" <> display a <>  "," <> display b <> ")"

-- | @since 0.0.1.0
instance (Display a, Display b, Display c) => Display (a, b, c) where
  display (a, b, c) = "(" <> display a <>  "," <> display b <> "," <> display c <> ")"

-- | @since 0.0.1.0
instance (Display a, Display b, Display c, Display d) => Display (a, b, c, d) where
  display (a, b, c, d) = "(" <> display a <>  "," <> display b <> "," <> display c <> "," <> display d <> ")"

-- $designChoices
--
-- === A “Lawless Typeclass”
--
-- The `Display` typeclass does not contain any law. This is a controversial choice for some people,
-- but the truth is that there are not any laws to ask of the consumer that are not already enforced
-- by the type system and the internals of the `Data.Text.Internal.Text` type.
--
-- === "🚫 You should not derive Display for function types!"
--
-- Sometimes, when using the library, you may encounter this message:
--
-- > • 🚫 You should not derive Display for function types!
-- >   💡 Write a 'newtype' wrapper that represents your domain more accurately.
-- >      If you are not consciously trying to use `display` on a function,
-- >      make sure that you are not missing an argument somewhere.
--
-- The `display` library does not allow the definition and usage of `Display` on
-- bare function types (@(a -> b)@).
-- Experience and time have shown that due to partial application being baked in the language,
-- many users encounter a partial application-related error message when a simple missing
-- argument to a function is the root cause.
--
-- There may be legitimate uses of a `Display` instance on a function type.
-- But these usages are extremely dependent on their domain of application.
-- That is why it is best to wrap them in a newtype that can better
-- express and enforce the domain.
--
-- === "🚫 You should not derive Display for ByteStrings!"
--
-- An arbitrary ByteStrings cannot be safely converted to text without prior knowledge of its encoding.
--
-- As such, in order to avoid dangerously blind conversions, it is recommended to use a specialised
-- function such as `decodeUtf8` or `decodeUtf8Strict` if you wish to turn a UTF8-encoded ByteString
-- to Text.
