{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
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
    display
  , Display(..)
  , -- * Deriving your instance automatically
    ShowInstance(..)
  , OpaqueInstance(..)
  , -- * Writing your instance by hand
    displayParen
  -- * Design choices
  -- $designChoices
  ) where

import Control.Exception hiding (TypeError)
import Data.ByteString
import Data.Int
import Data.Kind
import Data.List.NonEmpty
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Word
import GHC.Show (showLitString)
import GHC.TypeLits
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Builder.RealFloat as TB
import qualified Data.Text.Lazy as TL
import Data.Proxy

-- | A typeclass for user-facing output.
--
-- @since 0.0.1.0
class Display a where
  {-# MINIMAL displayBuilder | displayPrec #-}
  -- | Implement this method to describe how to convert your value to 'Builder'.
  displayBuilder :: a -> Builder
  displayBuilder = displayPrec 0

  -- | The method 'displayList' is provided to allow for a specialised
  -- way to render lists of a certain value.
  -- This is used to render the list of 'Char' as a string of characters
  -- enclosed in double quotes, rather than between square brackets and
  -- separated by commas.
  --
  -- === Example
  --
  -- > instance Display Char where
  -- >   displayBuilder '\'' = "'\\''"
  -- >   displayBuilder c = "'" <> TB.singleton c <> "\'"
  -- >   -- 'displayList' is overloaded, so that when the @Display [a]@ instance calls 'displayList',
  -- >   -- we end up with a nice string enclosed between double quotes.
  -- >   displayList cs = TB.fromString $ "\"" <> showLitString cs "\""
  --
  -- > instance Display a => Display [a] where
  -- >   -- In this instance, 'displayBuilder' is defined in terms of 'displayList', which for most types
  -- >   -- is defined as the default written in the class declaration.
  -- >   -- But when a ~ Char, there is an explicit implementation that is selected instead, which
  -- >   -- provides the rendering of the character string between double quotes.
  -- >   displayBuilder = displayList
  --
  -- ==== How implementations are selected
  --
  -- > displayBuilder ([1,2,3] :: [Int])
  -- > → displayBuilder @[Int] = displayBuilderList @Int
  -- > → Default `displayList`
  -- >
  -- > displayBuilder ("abc" :: [Char])
  -- > → displayBuilder @[Char] = displayBuilderList @Char
  -- > → Custom `displayList`
  displayList :: [a] -> Builder
  displayList [] = "[]"
  displayList (x:xs) = displayList' xs ("[" <> displayBuilder x)
    where
      displayList' :: [a] -> Builder -> Builder
      displayList' [] acc     = acc <> "]"
      displayList' (y:ys) acc = displayList' ys (acc <> "," <> displayBuilder y)

  -- | The method 'displayPrec' allows you to write instances that
  -- require nesting. The precedence parameter can be thought of as a
  -- suggestion coming from the surrounding context for how tightly to bind. If the precedence
  -- parameter is higher than the precedence of the operator (or constructor, function, etc.)
  -- being displayed, then that suggests that the output will need to be surrounded in parentheses
  -- in order to bind tightly enough (see 'displayParen').
  --
  -- For example, if an operator constructor is being displayed, then the precedence requirement
  -- for its arguments will be the precedence of the operator. Meaning, if the argument
  -- binds looser than the surrounding operator, then it will require parentheses.
  --
  -- Note that function/constructor application has an effective precedence of 10.
  --
  -- === Examples
  --
  -- > instance Display a => Display (Maybe a) where
  -- >   -- In this instance, we define 'displayPrec' rather than 'displayBuilder' as we need to decide
  -- >   -- whether or not to surround ourselves in parentheses based on the surrounding context.
  -- >   -- If the precedence parameter is higher than 10 (the precedence of constructor application)
  -- >   -- then we indeed need to surround ourselves in parentheses to avoid malformed outputs
  -- >   -- such as @Just Just 5@.
  -- >   -- We then set the precedence parameter of the inner 'displayPrec' to 11, as even
  -- >   -- constructor application is not strong enough to avoid parentheses.
  -- >   displayPrec _ Nothing = "Nothing"
  -- >   displayPrec prec (Just a) = displayParen (prec > 10) $ "Just " <> displayPrec 11 a
  --
  -- > data Pair a b = a :*: b
  -- > infix 5 :*: -- arbitrary choice of precedence
  -- > instance (Display a, Display b) => Display (Pair a b) where
  -- >   displayPrec prec (a :*: b) = displayParen (prec > 5) $ displayPrec 6 a <> " :*: " <> displayPrec 6 b
  displayPrec
    :: Int -- ^ The precedence level passed in by the surrounding context
    -> a
    -> Builder
  displayPrec _ = displayBuilder


-- | @since 0.0.1.0
-- Convert a value to a readable 'Text'.
--
-- === Examples
-- >>> display 3
-- "3"
--
-- >>> display True
-- "True"
--
display :: Display a => a -> Text
display a = TL.toStrict $ TB.toLazyText $ displayBuilder a

-- | 🚫 You should not derive Display for function types!
--
-- 💡 Write a 'newtype' wrapper that represents your domain more accurately.
--    If you are not consciously trying to use `display` on a function,
--    make sure that you are not missing an argument somewhere.
--
-- @since 0.0.1.0
instance CannotDisplayBareFunctions => Display (a -> b) where
  displayBuilder = undefined

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
-- Use 'decodeUtf8'' or 'decodeUtf8With' to convert from UTF-8
--
-- @since 0.0.1.0
instance CannotDisplayByteStrings => Display ByteString where
  displayBuilder = undefined

-- | 🚫 You should not derive Display for lazy ByteStrings!
--
-- 💡 Always provide an explicit encoding.
-- Use 'decodeUtf8'' or 'decodeUtf8With' to convert from UTF-8
--
-- @since 0.0.1.0
instance CannotDisplayByteStrings => Display BL.ByteString where
  displayBuilder = undefined

type family CannotDisplayByteStrings :: Constraint where
  CannotDisplayByteStrings = TypeError
    ( 'Text "🚫 You should not derive Display for ByteStrings!" ':$$:
      'Text "💡 Always provide an explicit encoding" ':$$:
      'Text     "Use 'decodeUtf8'' or 'decodeUtf8With' to convert from UTF-8"
    )

-- | @since 0.0.1.0
-- A utility function that surrounds the given 'Builder' with parentheses when the Bool parameter is True.
-- Useful for writing instances that may require nesting. See the 'displayPrec' documentation for more
-- information.
displayParen :: Bool -> Builder -> Builder
displayParen b txt = if b then "(" <> txt <> ")" else txt

-- | This wrapper allows you to create an opaque instance for your type,
-- useful for redacting sensitive content like tokens or passwords.
--
-- === Example
--
-- > data UserToken = UserToken UUID
-- >  deriving Display
-- >    via (OpaqueInstance "[REDACTED]" UserToken)
--
-- > display $ UserToken "7a01d2ce-31ff-11ec-8c10-5405db82c3cd"
-- > "[REDACTED]"
-- @since 0.0.1.0
--
newtype OpaqueInstance (str :: Symbol) (a :: Type) = Opaque a

instance KnownSymbol str => Display (OpaqueInstance str a) where
  displayBuilder _ = TB.fromString $ symbolVal (Proxy @str)
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
newtype ShowInstance (a :: Type)
  = ShowInstance a
  deriving newtype
    ( Show -- ^ @since 0.0.1.0
    )

-- | This wrapper allows you to rely on a pre-existing 'Show' instance in order to derive 'Display' from it.
--
-- @since 0.0.1.0
instance Show e => Display (ShowInstance e) where
  displayBuilder s = TB.fromString $ show s

-- @since 0.0.1.0
newtype DisplayDecimal e
  = DisplayDecimal e
  deriving newtype
    (Integral, Real, Enum, Ord, Num, Eq)

-- @since 0.0.1.0
instance Integral e => Display (DisplayDecimal e) where
  displayBuilder = TB.decimal

-- @since 0.0.1.0
newtype DisplayRealFloat e
  = DisplayRealFloat e
  deriving newtype
    (RealFloat, RealFrac, Real, Ord, Eq, Num, Fractional, Floating)

-- @since 0.0.1.0
instance RealFloat e => Display (DisplayRealFloat e) where
  displayBuilder = TB.realFloat

-- | @since 0.0.1.0
deriving via (ShowInstance ()) instance Display ()

-- | @since 0.0.1.0
deriving via (ShowInstance Bool) instance Display Bool

-- | @since 0.0.1.0
instance Display Char where
  displayBuilder '\'' = "'\\''"
  displayBuilder c = "'" <> TB.singleton c <> "\'"
  -- 'displayList' is overloaded, so that when the @Display [a]@ instance calls 'displayList',
  -- we end up with a nice string enclosed between double quotes.
  displayList cs = TB.fromString $ "\"" <> showLitString cs "\""

-- | Lazy 'TL.Text'
--
-- @since 0.0.1.0
instance Display TL.Text where
  displayBuilder = TB.fromLazyText

-- | Strict 'Data.Text.Text'
--
-- @since 0.0.1.0
instance Display Text where
  displayBuilder = TB.fromText

-- | @since 0.0.1.0
instance Display a => Display [a] where
  {-# SPECIALISE instance Display [String] #-}
  {-# SPECIALISE instance Display [Char] #-}
  {-# SPECIALISE instance Display [Int] #-}
  -- In this instance, 'displayBuilder' is defined in terms of 'displayList', which for most types
  -- is defined as the default written in the class declaration.
  -- But when @a ~ Char@, there is an explicit implementation that is selected instead, which
  -- provides the rendering of the character string between double quotes.
  displayBuilder = displayList

-- | @since 0.0.1.0
instance Display a => Display (NonEmpty a) where
  displayBuilder (a :| as) = displayBuilder a <> TB.fromString " :| " <> displayBuilder as

-- | @since 0.0.1.0
instance Display a => Display (Maybe a) where
  -- In this instance, we define 'displayPrec' rather than 'displayBuilder' as we need to decide
  -- whether or not to surround ourselves in parentheses based on the surrounding context.
  -- If the precedence parameter is higher than 10 (the precedence of constructor application)
  -- then we indeed need to surround ourselves in parentheses to avoid malformed outputs
  -- such as @Just Just 5@.
  -- We then set the precedence parameter of the inner 'displayPrec' to 11, as even
  -- constructor application is not strong enough to avoid parentheses.
  displayPrec _ Nothing = "Nothing"
  displayPrec prec (Just a) = displayParen (prec > 10) $ "Just " <> displayPrec 11 a

-- | @since 0.0.1.0
deriving via (DisplayRealFloat Double) instance Display Double

-- | @since 0.0.1.0
deriving via (DisplayRealFloat Float) instance Display Float

-- | @since 0.0.1.0
deriving via (DisplayDecimal Int) instance Display Int

-- | @since 0.0.1.0
deriving via (DisplayDecimal Int8) instance Display Int8

-- | @since 0.0.1.0
deriving via (DisplayDecimal Int16) instance Display Int16

-- | @since 0.0.1.0
deriving via (DisplayDecimal Int32) instance Display Int32

-- | @since 0.0.1.0
deriving via (DisplayDecimal Int64) instance Display Int64

-- | @since 0.0.1.0
deriving via (DisplayDecimal Integer) instance Display Integer

-- | @since 0.0.1.0
deriving via (DisplayDecimal Word) instance Display Word

-- | @since 0.0.1.0
deriving via (DisplayDecimal Word8) instance Display Word8

-- | @since 0.0.1.0
deriving via (DisplayDecimal Word16) instance Display Word16

-- | @since 0.0.1.0
deriving via (DisplayDecimal Word32) instance Display Word32

-- | @since 0.0.1.0
deriving via (DisplayDecimal Word64) instance Display Word64

-- | @since 0.0.1.0
deriving via (ShowInstance IOException) instance Display IOException

-- | @since 0.0.1.0
deriving via (ShowInstance SomeException) instance Display SomeException

-- | @since 0.0.1.0
instance (Display a, Display b) => Display (a, b) where
  displayBuilder (a, b) = "(" <> displayBuilder a <>  "," <> displayBuilder b <> ")"

-- | @since 0.0.1.0
instance (Display a, Display b, Display c) => Display (a, b, c) where
  displayBuilder (a, b, c) = "(" <> displayBuilder a <>  "," <> displayBuilder b <> "," <> displayBuilder c <> ")"

-- | @since 0.0.1.0
instance (Display a, Display b, Display c, Display d) => Display (a, b, c, d) where
  displayBuilder (a, b, c, d) = "(" <> displayBuilder a <>  "," <> displayBuilder b <> "," <> displayBuilder c <> "," <> displayBuilder d <> ")"

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
-- function such as `decodeUtf8'` or `decodeUtf8With` if you wish to turn a UTF8-encoded ByteString
-- to Text.
