{-# LANGUAGE FlexibleInstances #-}

module Data.Gogol.DatastoreInstances where

import           Control.Lens
import           Data.ByteString            (ByteString)
import           Data.Gogol.DatastoreEntity
import qualified Data.HashMap.Lazy          as HM
import           Data.Int                   (Int32, Int64)
import           Data.List.NonEmpty         (NonEmpty, toList)
import           Data.Text                  (Text, pack)
import           Data.Time.Clock            (UTCTime)
import           GHC.Float                  (float2Double)
import           Network.Google.Datastore   (Entity, Key, LatLng,
                                             ValueNullValue (NullValue),
                                             arrayValue, avValues, eKey,
                                             eProperties, entity,
                                             entityProperties, kPath, key,
                                             pathElement, peKind, vArrayValue,
                                             vBlobValue, vBooleanValue,
                                             vDoubleValue, vGeoPointValue,
                                             vIntegerValue, vKeyValue,
                                             vNullValue, vStringValue,
                                             vTimestampValue, value)


_AsDatastoreEntity :: DatastoreEntity a => Getter a (Maybe Entity)
_AsDatastoreEntity = to (toEntity . entity')
    where toEntity (Entity k params) =
            pure $ entity & eKey ?~ (key & kPath .~ [pathElement & peKind ?~ k])
                          & eProperties ?~ entityProperties (HM.fromList params)
          toEntity x                 = Nothing

instance DatastoreEntity Bool where
  entity' b = V $ value & vBooleanValue ?~ b

instance DatastoreEntity Int where
  entity' i = V $ value & vIntegerValue ?~ fromIntegral i

instance DatastoreEntity Integer where
  entity' i = V $ value & vIntegerValue ?~ fromIntegral i

instance DatastoreEntity Int32 where
  entity' i = V $ value & vIntegerValue ?~ fromIntegral i

instance DatastoreEntity Int64 where
  entity' i = V $ value & vIntegerValue ?~ i

instance DatastoreEntity Float where
  entity' f = V $ value & vDoubleValue ?~ float2Double f

instance DatastoreEntity Double where
  entity' d = V $ value & vDoubleValue ?~ d

instance DatastoreEntity ByteString where
  entity' b = V $ value & vBlobValue ?~ b

instance DatastoreEntity Text where
  entity' x = V $ value & vStringValue ?~ x

instance DatastoreEntity Key where
  entity' k = V $ value & vKeyValue ?~ k

instance DatastoreEntity UTCTime where
  entity' t = V $ value & vTimestampValue ?~ t

instance DatastoreEntity LatLng where
  entity' l = V $ value & vGeoPointValue ?~ l

instance DatastoreEntity a => DatastoreEntity (NonEmpty a) where
  entity' xs = V $ value & vArrayValue ?~ (arrayValue & avValues .~
                                             (toList xs >>= indiv))
    where indiv x = case entity' x of
            V v -> [v]
            _   -> []

instance DatastoreEntity a => DatastoreEntity [a] where
  entity' xs = V $ value & vArrayValue ?~ (arrayValue & avValues .~
                                             (xs >>= indiv))
    where indiv x = case entity' x of
            V v -> [v]
            _   -> []

instance DatastoreEntity a => DatastoreEntity (Maybe a) where
  entity' (Just x) = entity' x
  entity' Nothing  = V $ value & vNullValue ?~ NullValue

instance DatastoreEntity b => DatastoreEntity (Either a b) where
  entity' (Right x) = entity' x
  entity' (Left _)  = V $ value & vNullValue ?~ NullValue

