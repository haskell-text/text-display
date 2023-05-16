{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
--  Module      : Data.Text.Display.Generic
--  Copyright   : © Jonathan Lorimer, 2023
--  License     : MIT
--  Maintainer  : jonathanlorimer@pm.me
--  Stability   : stable
--
--  Generic machinery for automatically deriving display instances for record types
module Data.Text.Display.Generic where

import Data.Kind
import Data.Text.Display.Core
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import Data.Type.Bool
import GHC.Generics
import GHC.TypeLits

-- | Generic typeclass machinery for inducting on the structure
-- of the type, such that we can thread `Display` instances through
-- the structure of the type. The primary use case is for implementing
-- `RecordInstance`, which does this "threading" for record fields. This
-- machinery does, crucially, depend on child types (i.e. the type of a
-- record field) having a `Display` instance.
--
-- @since 0.0.5.0
class GDisplay1 f where
  gdisplayBuilder1 :: f p -> Builder

instance GDisplay1 V1 where
  gdisplayBuilder1 x = case x of {}

instance GDisplay1 U1 where
  gdisplayBuilder1 _ = "()"

-- | This is the most important instance, it can be considered as the "base case". It
-- requires a non-generic `Display` instance. All this generic machinery can be conceptualized
-- as distributing these `displayBuilder` calls across a product type.
instance Display c => GDisplay1 (K1 i c) where
  gdisplayBuilder1 (K1 a) = displayBuilder a

instance (Constructor c, GDisplay1 f) => GDisplay1 (M1 C c f) where
  gdisplayBuilder1 c@(M1 a)
    | conIsRecord c = TB.fromString (conName c) <> "\n  { " <> gdisplayBuilder1 a <> "\n  }"
    | conIsTuple c = TB.fromString (conName c) <> " ( " <> gdisplayBuilder1 a <> " )"
    | otherwise = TB.fromString (conName c) <> " " <> gdisplayBuilder1 a
    where
      conIsTuple :: C1 c f p -> Bool
      conIsTuple y =
        tupleName (conName y)
        where
          tupleName ('(' : ',' : _) = True
          tupleName _ = False

instance (Selector s, GDisplay1 f) => GDisplay1 (M1 S s f) where
  gdisplayBuilder1 s@(M1 a) =
    if selName s == ""
      then gdisplayBuilder1 a
      else TB.fromString (selName s) <> " = " <> gdisplayBuilder1 a

instance GDisplay1 f => GDisplay1 (M1 D s f) where
  gdisplayBuilder1 (M1 a) = gdisplayBuilder1 a

instance (GDisplay1 a, GDisplay1 b) => GDisplay1 (a :*: b) where
  gdisplayBuilder1 (a :*: b) = gdisplayBuilder1 a <> "\n  , " <> gdisplayBuilder1 b

instance (GDisplay1 a, GDisplay1 b) => GDisplay1 (a :+: b) where
  gdisplayBuilder1 (L1 a) = gdisplayBuilder1 a
  gdisplayBuilder1 (R1 b) = gdisplayBuilder1 b

gdisplayBuilderDefault :: (Generic a, GDisplay1 (Rep a)) => a -> Builder
gdisplayBuilderDefault = gdisplayBuilder1 . from

-- | This wrapper allows you to create an `Display` instance for a record,
-- so long as all the record fields have a `Display` instance as well.
--
-- === Example
--
-- > data Password = Password
-- >  deriving Display
-- >    via (OpaqueInstance "[REDACTED]" Password)
--
-- > data MyRecord =
-- >    MyRecord
-- >      { fieldA :: String
-- >      , fieldB :: Maybe String
-- >      , fieldC :: Int
-- >      , pword :: Password
-- >      }
-- >      deriving stock (Generic)
-- >      deriving (Display) via (RecordInstance MyRecord)
--
-- > putStrLn . Data.Text.unpack . display $ MyRecord "hello" (Just "world") 22 Password
--
-- > MyRecord
-- >   { fieldA = hello
-- >   , fieldB = Just world
-- >   , fieldC = 22
-- >   , pword = [REDACTED]
-- >   }
--
-- @since 0.0.5.0
newtype RecordInstance a = RecordInstance {unDisplayProduct :: a}

instance Generic a => Generic (RecordInstance a) where
  type Rep (RecordInstance a) = Rep a
  to = RecordInstance . to
  from (RecordInstance x) = from x

-- | We leverage the `AssertNoSum` type family to prevent consumers
-- from deriving instances for sum types. Sum types should use a manual instance
-- or derive one via `ShowInstance`.
--
-- @since 0.0.5.0
instance (AssertNoSumRecordInstance Display a, Generic a, GDisplay1 (Rep a)) => Display (RecordInstance a) where
  displayBuilder = gdisplayBuilderDefault

-- | This type family is lifted from generic-data. We use it to prevent the user from
-- deriving a `RecordInstance` for sum types
--
-- @since 0.0.5.0
type family HasSum f where
  HasSum V1 = 'False
  HasSum U1 = 'False
  HasSum (K1 i c) = 'False
  HasSum (M1 i c f) = HasSum f
  HasSum (f :*: g) = HasSum f || HasSum g
  HasSum (f :+: g) = 'True

class Assert (pred :: Bool) (msg :: ErrorMessage)
instance Assert 'True msg
instance TypeError msg ~ '() => Assert 'False msg

-- | Constraint to prevent misuse of `RecordInstance` deriving via mechanism.
--
-- === Example
--
-- > data MySum = A | B | C deriving stock (Generic) deriving (Display) via (RecordInstance MySum)
--
-- >    • 🚫 Cannot derive Display instance for MySum via RecordInstance due to sum type
-- >      💡 Sum types should use a manual instance or derive one via ShowInstance.
-- >    • When deriving the instance for (Display MySum)
--
-- @since 0.0.5.0
type AssertNoSumRecordInstance (constraint :: Type -> Constraint) a =
  Assert
    (Not (HasSum (Rep a)))
    ( 'Text "🚫 Cannot derive "
        ':<>: 'ShowType constraint
        ':<>: 'Text " instance for "
        ':<>: 'ShowType a
        ':<>: 'Text " via RecordInstance due to sum type"
        ':$$: 'Text "💡 Sum types should use a manual instance or derive one via ShowInstance."
    )
