# NoSQL Generic

## Overview

Haskell generic (de)serialization typeclass derivation of popular Cloud NoSQL database type conversion functions for record types, making use of **GHC.Generics**.

It seemed like a potentially useful idea given how complicated the cloud service NoSQL database datatypes are, and how much time is spent in SOAs marshalling data between various different representations for networking and storage.

Presently targeting and tested on **GHC 7.10.3**.

- Supports: [Gogol Datastore](https://hackage.haskell.org/package/gogol-datastore) generic record-type Datastore entity serialization and deserialization.
  - That is, it automatically generates functions to convert between types `a -> (Maybe Entity)` and backwards from `Entity -> Maybe a`, where there is an instance of `Generic` in scope.
  - It makes heavy use of [lens](https://hackage.haskell.org/package/lens) as the Gogol library depends on it for all data work.
- Future: [Amazonka DynamoDB](https://hackage.haskell.org/package/amazonka-dynamodb).

## Example

```haskell
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens               ((^.))
import           Data.Gogol.DatastoreEntity
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Network.Google.Datastore   (Entity)

newtype Foo = Foo { _foo' :: Text } deriving (DatastoreEntity, Eq, Generic, Show)

data MyType
  = MyConstructor
    { _foo :: Foo
    , _bar :: Bool
    , _baz :: Maybe MyType
    }
  | MySndConstructor
    { _quux :: Int
    }
  deriving (DatastoreEntity, Eq, Generic, Show)

myType :: MyType
myType = MyConstructor (Foo "hello") True Nothing

myTypeAsEntity :: Maybe Entity
myTypeAsEntity = myType^._ToDatastoreEntity

myType2 :: Maybe Entity
myType2 = MySndConstructor 3 ^._ToDatastoreEntity

myType3RoundTrip :: Maybe Bool -- Evaluates to (Just True)
myType3RoundTrup = do
  let x = MyConstructor (Foo "yo") False (pure $ MySndConstructor 1)
  entity'' <- x^._ToDatastoreEntity
  x''      <- entity''^._FromDatastoreEntity
  -- forwards / backwards serialization -> deserialization from Gogol Datastore Entity type
  return $ x == x''
```
