-- |
--  Module      : Data.Text.Display
--  Copyright   : © Hécate Moonlight, 2021
--  License     : MIT
--  Maintainer  : hecate@glitchbra.in
--  Stability   : stable
--
--  Use 'display' to produce user-facing text
module Data.Text.Display
  ( -- * Documentation
    display
  , Display (..)

    -- * Deriving your instance automatically
  , ShowInstance (..)
  , OpaqueInstance (..)
  , RecordInstance (..)

    -- * Writing your instance by hand
  , displayParen

    -- * Design choices
    -- $designChoices
  )
where

import Data.Text.Display.Core
import Data.Text.Display.Generic

-- $designChoices
--
-- === A “Lawless Typeclass”
--
-- The 'Display' typeclass does not contain any law. This is a controversial choice for some people,
-- but the truth is that there are not any laws to ask of the consumer that are not already enforced
-- by the type system and the internals of the 'Data.Text.Internal.Text' type.
--
-- === "🚫 You should not try to display functions!"
--
-- Sometimes, when using the library, you may encounter this message:
--
-- > • 🚫 You should not try to display functions!
-- >   💡 Write a 'newtype' wrapper that represents your domain more accurately.
-- >      If you are not consciously trying to use `display` on a function,
-- >      make sure that you are not missing an argument somewhere.
--
-- The 'display' library does not allow the definition and usage of 'Display' on
-- bare function types (@(a -> b)@).
-- Experience and time have shown that due to partial application being baked in the language,
-- many users encounter a partial application-related error message when a simple missing
-- argument to a function is the root cause.
--
-- There may be legitimate uses of a 'Display' instance on a function type.
-- But these usages are extremely dependent on their domain of application.
-- That is why it is best to wrap them in a newtype that can better
-- express and enforce the domain.
--
-- === "🚫 You should not try to display ByteStrings!"
--
-- An arbitrary ByteStrings cannot be safely converted to text without prior knowledge of its encoding.
--
-- As such, in order to avoid dangerously blind conversions, it is recommended to use a specialised
-- function such as 'Data.Text.Encoding.decodeUtf8'' or 'Data.Text.Encoding.decodeUtf8With' if you wish to turn a UTF8-encoded ByteString
-- to Text.
