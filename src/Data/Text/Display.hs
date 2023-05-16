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
