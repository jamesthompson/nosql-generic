{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Gogol.DatastoreEntityTests where

import           Control.Lens               (view, (&), (?~), (^.))
import           Data.Gogol.DatastoreEntity
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Network.Google.Datastore   (vStringValue, value)
import           Test.QuickCheck            hiding (output)
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck      hiding (output)
import           Test.Tasty.TH

data RecordTest1
  = RecordTest1
    { _a :: Text
    , _b :: Bool
    , _c :: Int
    } deriving (Eq, Show, Generic, DatastoreEntity)

case_simple_record_forwards_backwards_serialization =
  let exp = RecordTest1 "foo" True 0
      res = exp^._ToDatastoreEntity >>= view _FromDatastoreEntity
  in res @?= Just exp

data RecordTest2
  = RecordTest2
    { _d :: Text
    , _e :: Bool
    , _f :: RecordTest1
    } deriving (Eq, Show, Generic, DatastoreEntity)

case_nested_simple_records_forwards_backwards_serialization =
  let exp = RecordTest2 "bar" False (RecordTest1 "foo" True 0)
      res = exp^._ToDatastoreEntity >>= view _FromDatastoreEntity
  in res @?= Just exp

data RecordTest3
  = RecordTest3
    { _g :: Text
    , _h :: Bool
    , _i :: Maybe RecordTest3
    } deriving (Eq, Show, Generic, DatastoreEntity)

case_recursive_optional_record_forwards_backwards_serialization_1 =
  let exp = RecordTest3 "baz" True Nothing
      res = exp^._ToDatastoreEntity >>= view _FromDatastoreEntity
  in res @?= Just exp

case_recursive_optional_record_forwards_backwards_serialization_2 =
  let exp = RecordTest3 "baz" True (pure $ RecordTest3 "quux" False Nothing)
      res = exp^._ToDatastoreEntity >>= view _FromDatastoreEntity
  in res @?= Just exp

data RecordTest4
  = RecordTest4
    { _j :: Text
    , _k :: Bool
    , _l :: [Int]
    } deriving (Eq, Show, Generic, DatastoreEntity)

case_list_primitive_field_simple_record_forwards_backwards_serialization_1 =
  let exp = RecordTest4 "foo" True []
      res = exp^._ToDatastoreEntity >>= view _FromDatastoreEntity
  in res @?= Just exp

case_list_primitive_field_simple_record_forwards_backwards_serialization_2 =
  let exp = RecordTest4 "foo" True [1,2,3]
      res = exp^._ToDatastoreEntity >>= view _FromDatastoreEntity
  in res @?= Just exp

data RecordTest5
  = RecordTest5
    { _m :: Text
    , _n :: Bool
    , _o :: [RecordTest5]
    } deriving (Eq, Show, Generic, DatastoreEntity)

case_list_record_field_record_forwards_backwards_serialization_1 =
  let exp = RecordTest5 "foo" True []
      res = exp^._ToDatastoreEntity >>= view _FromDatastoreEntity
  in res @?= Just exp

case_list_record_field_record_forwards_backwards_serialization_2 =
  let exp = RecordTest5 "foo" True [RecordTest5 "bar" False []]
      res = exp^._ToDatastoreEntity >>= view _FromDatastoreEntity
  in res @?= Just exp

data RecordTest6
  = RecordTest6A
    { _p :: Text
    , _q :: Bool
    }
  | RecordTest6B
    { _r :: Int
    , _s :: Text
    }
  deriving (Eq, Show, Generic, DatastoreEntity)

case_multiple_record_constructors_forwards_backwards_serialization_1 =
  let exp = RecordTest6A "foo" True
      res = exp^._ToDatastoreEntity >>= view _FromDatastoreEntity
  in res @?= Just exp

case_multiple_record_constructors_forwards_backwards_serialization_2 =
  let exp = RecordTest6B 1 "bar"
      res = exp^._ToDatastoreEntity >>= view _FromDatastoreEntity
  in res @?= Just exp

data Colour
  = Red
  | Green
  | Blue
  deriving (Eq, Show, Generic)

instance DatastoreEntity Colour where
  toEntity Red   = V $ value & vStringValue ?~ "Red"
  toEntity Green = V $ value & vStringValue ?~ "Green"
  toEntity Blue  = V $ value & vStringValue ?~ "Blue"
  fromEntity (V v) = case v^.vStringValue of
    Just "Red"   -> pure Red
    Just "Green" -> pure Green
    Just "Blue"  -> pure Blue
    Just _       -> Nothing
  fromEntity _     = Nothing

data RecordTest7
  = RecordTest7
    { _t :: Text
    , _u :: Colour
    } deriving (Eq, Show, Generic, DatastoreEntity)

case_record_with_sum_type_field_forwards_backwards_serialization =
  let exp = RecordTest7 "sum" Green
      res = exp^._ToDatastoreEntity >>= view _FromDatastoreEntity
  in res @?= Just exp

data RecordTest8
  = RecordTest8
    { _v :: Text
    , _w :: [Maybe RecordTest8]
    } deriving (Eq, Show, Generic, DatastoreEntity)

case_list_with_maybe_record_field_record_forwards_backwards_serialization_1 =
  let exp = RecordTest8 "foo" []
      res = exp^._ToDatastoreEntity >>= view _FromDatastoreEntity
  in res @?= Just exp

case_list_with_maybe_record_field_record_forwards_backwards_serialization_2 =
  let exp = RecordTest8 "bar" [Nothing]
      res = exp^._ToDatastoreEntity >>= view _FromDatastoreEntity
  in res @?= Just exp

case_list_with_maybe_record_field_record_forwards_backwards_serialization_3 =
  let exp = RecordTest8 "bar" [pure $ RecordTest8 "baz" []]
      res = exp^._ToDatastoreEntity >>= view _FromDatastoreEntity
  in res @?= Just exp

case_list_with_maybe_record_field_record_forwards_backwards_serialization_4 =
  let exp = RecordTest8 "bar" [pure $ RecordTest8 "baz" [Nothing]]
      res = exp^._ToDatastoreEntity >>= view _FromDatastoreEntity
  in res @?= Just exp

case_list_with_maybe_record_field_record_forwards_backwards_serialization_5 =
  let exp = RecordTest8 "bar" [pure $ RecordTest8 "baz" [pure $ RecordTest8 "quux" []]]
      res = exp^._ToDatastoreEntity >>= view _FromDatastoreEntity
  in res @?= Just exp

datastoreEntityTests :: TestTree
datastoreEntityTests = $(testGroupGenerator)

