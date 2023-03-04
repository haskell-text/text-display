-- ## pg-entity tutorial
-- In this tutorial, you will learn how to implement the Entity typeclass for your business logic data-types, and run
-- queries against the database.
--
-- ### Setting up
-- #### Language Extensions
--
-- * `OverloadedLists` allow us to use the `[list]` syntax for datatypes other than List, like Vector.
-- * `QuasiQuotes` enable us to write plain SQL and field names in a `[|quasi-quoter block|]`.
-- * The Deriving extensions give us more powerful typeclass derivation.
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Tutorial where

-- Then, let's import some data-types and modules:
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transact (DBT)
import GHC.Generics

import Control.Monad.Error
import qualified Data.Time as Time
import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Entity.Types

-- And let's write down our initial data models for a blog. `Author`, and `BlogPost`.

-- It is good practice to wrap your primary key in a newtype to gain more
-- type-safety, but without losing access to typeclass instances of the
-- underlying type, with `deriving newtype`.
newtype AuthorId = AuthorId {getAuthorId :: UUID}
  deriving newtype (Eq, Show, FromField, ToField)

data Author = Author
  { authorId :: AuthorId
  -- ^ Primary key
  , name :: Text
  , createdAt :: UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)

-- and

newtype BlogPostId = BlogPostId {getBlogPostId :: UUID}
  deriving newtype (Eq, FromField, Show, ToField)

data BlogPost = BlogPost
  { blogPostId :: BlogPostId
  -- ^ Primary key
  , authorId :: AuthorId
  -- ^ Foreign key
  , title :: Text
  , content :: Text
  , createdAt :: UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)

-- Let us write the `Entity` instances now:

instance Entity Author where
  tableName = "authors"
  primaryKey = [field| author_id |]
  fields =
    [ [field| author_id |]
    , [field| name |]
    , [field| created_at |]
    ]

-- The above instance declaration reads as:
-- > My table's name is _authors_, its primary key is _author\_id_, and the fields are _author\_id_, _name_, and _created\_at_.

-- The order matters for the declarations, so make sure that each field is at the correct position.

-- Let's do the same for `BlogPost`:

instance Entity BlogPost where
  tableName = "blogposts"
  primaryKey = [field| blogpost_id |]
  fields =
    [ [field| blogpost_id |]
    , [field| author_id |]
    , [field| title |]
    , [field| content |]
    , [field| created_at |]
    ]

-- And these instances will give you access to the Entity functions to query your tables

-- ### Using Generics

-- But all these manual instances are a tad tedious. Fortunately, there is a derivation mechanism available that allows you
-- to automate the generation of the `Entity` instance. This mechanism is called `DerivingVia`.
-- It allows you to use a wrapper, called `GenericEntity`, and a list of options at the type-level to generate the instance

-- ```haskell
-- deriving via
--   (GenericEntity
--     '[ TableName "authors"
--      , PrimaryKey "author_id"
--      ] Author
--   ) instance Entity Author
-- ```

-- Those two options, `TableName` and `PrimaryKey` are optional, and the library will adopt the following defaults:

-- * `tableName` will be the snake\_case version of the record's name. No pluralisation will be done.

-- * `primaryKey` will be the snake\_case version of the first field of the record.

-- * `field` names will be the snake\_case version of the record fields.

-- The advantages brought by this technique are that the error surface is reduced when implementing the `Entity` typeclass.

-- ⚠️  While all these deriving parameters are optional, I advise you to write down the table name (with the `TableName` option).
-- In the PostgreSQL world, it is customary to pluralise the name of the table containing your data, and
-- `pg-entity` does not automatically do this.

-- ⚠️  For performance reasons, you are advised to write the primary key in the options. If you do not, the generic deriving mechanism will have to do a linear
-- search in the fields list (of complexity 𝛰(n) whereas the lookup will take 𝛰(1) if the primary key is given in your code. Do this especially if you have
-- long tables.

-- Do it if you have long tables.

-- ### Writing the SQL migrations

-- In a separate file, we will translate our Haskell data models into SQL migrations.

-- ```sql
-- create table authors (
--     author_id uuid primary key,
--     name text not null,
--     created_at timestamptz not null
--     );

-- create table blogposts (
--     blogpost_id uuid primary key,
--     author_id uuid not null,
--     title text not null,
--     content text not null,
--     created_at timestamptz not null,
--     constraint fk_author
--       foreign key(author_id)
--         references authors(author_id)
--     );
-- ```

-- ### Making queries

-- By implementing the `Entity` Typeclass, your data-type has access to a variety of functions, combinators and helpers that serve the one true purpose of
-- this library:

-- > Provide a safe mechanism to expand the fields of a table while writing a query.

-- Let us define our insertion function for the Author model:

insertAuthor :: Author -> DBT IO ()
insertAuthor = insert @Author

-- The result of this function, which we call a “DBT action”,*
-- is then passed to [`withPool`](https://hackage.haskell.org/package/pg-entity-0.0.1.0/docs/Database-PostgreSQL-Entity-DBT.html#v:withPool).

-- You can then build a higher-level API endpoint or route controller like that:

data AuthorInfo = AuthorInfo
  { name :: Text
  }
  deriving stock (Eq, Show)

mkAuthor :: MonadIO m => AuthorInfo -> m Author
mkAuthor AuthorInfo{name = authorName} = do
  authorId <- liftIO $ AuthorId <$> UUID.nextRandom
  createdAt <- liftIO Time.getCurrentTime
  pure Author{name = authorName, authorId, createdAt}

addAuthor
  :: (MonadIO m)
  => Pool Connection
  -> AuthorInfo
  -> m ()
addAuthor pool info = do
  newAuthor <- mkAuthor info
  withPool pool $ insertAuthor newAuthor

-- And if you want to later select an `Author` based on its `AuthorId`:

getAuthor
  :: (MonadIO m)
  => Pool Connection
  -> AuthorId
  -> m (Maybe Author)
getAuthor pool authorId =
  withPool pool $ selectOneByField [field| author_id |] (Only authorId)

-- This is the end of this tutorial. There are many more functions to discover that will help you write your queries.
-- While you shouldn't have to explore the source code to gain understanding of how to use the library, feel free to
-- navigate it to see how the underlying mechanisms work. :)

-- Next, you can consult our Guides section to see how to do error handling.
