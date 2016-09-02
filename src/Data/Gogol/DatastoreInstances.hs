{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Gogol.DatastoreInstances where

import           Control.Applicative        ((<|>))
import           Control.Lens
import           Data.ByteString            (ByteString)
import           Data.Gogol.DatastoreEntity
import qualified Data.HashMap.Lazy          as HM
import           Data.Int                   (Int32, Int64)
import           Data.List.NonEmpty         (NonEmpty, fromList, toList)
import           Data.Text                  (Text, pack)
import           Data.Time.Clock            (UTCTime)
import           GHC.Float                  (double2Float, float2Double)
import           GHC.Generics               (Generic)
import           Network.Google.Datastore   (Entity, Key, LatLng,
                                             ValueNullValue (NullValue),
                                             arrayValue, avValues, eKey,
                                             eProperties, entity,
                                             entityProperties, epAddtional,
                                             kPath, key, pathElement, peKind,
                                             vArrayValue, vBlobValue,
                                             vBooleanValue, vDoubleValue,
                                             vGeoPointValue, vIntegerValue,
                                             vKeyValue, vNullValue,
                                             vStringValue, vTimestampValue,
                                             value)


_AsDatastoreEntity :: DatastoreEntity a => Getter a (Maybe Entity)
_AsDatastoreEntity = to (toEntity' . toEntity)
  where toEntity' (EntityC k params) =
          pure $ entity & eKey ?~ (key & kPath .~ [pathElement & peKind ?~ k])
                        & eProperties ?~ entityProperties (HM.fromList params)
        toEntity' x                 = Nothing

_AsEntityTransform :: Getter Entity (Maybe EntityTransform)
_AsEntityTransform = to toTransform
  where toTransform :: Entity -> Maybe EntityTransform
        toTransform e = EntityC <$> (view kPath <$> e^.eKey)^._Just._head.peKind
                                <*> (HM.toList <$> (e^?eProperties._Just.epAddtional))

instance DatastoreEntity Bool where
  toEntity b = V $ value & vBooleanValue ?~ b
  fromEntity (V v) = v^.vBooleanValue
  fromEntity _     = Nothing

instance DatastoreEntity Int where
  toEntity i = V $ value & vIntegerValue ?~ fromIntegral i
  fromEntity (V v) = fromIntegral <$> v^.vIntegerValue
  fromEntity _     = Nothing

instance DatastoreEntity Integer where
  toEntity i = V $ value & vIntegerValue ?~ fromIntegral i
  fromEntity (V v) = fromIntegral <$> v^.vIntegerValue
  fromEntity _     = Nothing

instance DatastoreEntity Int32 where
  toEntity i = V $ value & vIntegerValue ?~ fromIntegral i
  fromEntity (V v) = fromIntegral <$> v^.vIntegerValue
  fromEntity _     = Nothing

instance DatastoreEntity Int64 where
  toEntity i = V $ value & vIntegerValue ?~ i
  fromEntity (V v) = v^.vIntegerValue
  fromEntity _     = Nothing

instance DatastoreEntity Float where
  toEntity f = V $ value & vDoubleValue ?~ float2Double f
  fromEntity (V v) = double2Float <$> v^.vDoubleValue
  fromEntity _     = Nothing

instance DatastoreEntity Double where
  toEntity d = V $ value & vDoubleValue ?~ d
  fromEntity (V v) = v^.vDoubleValue
  fromEntity _     = Nothing

instance DatastoreEntity ByteString where
  toEntity b = V $ value & vBlobValue ?~ b
  fromEntity (V v) = v^.vBlobValue
  fromEntity _     = Nothing

instance DatastoreEntity Text where
  toEntity x = V $ value & vStringValue ?~ x
  fromEntity (V v) = v^.vStringValue
  fromEntity _     = Nothing

instance DatastoreEntity Key where
  toEntity k = V $ value & vKeyValue ?~ k
  fromEntity (V v) = v^.vKeyValue
  fromEntity _     = Nothing

instance DatastoreEntity UTCTime where
  toEntity t = V $ value & vTimestampValue ?~ t
  fromEntity (V v) = v^.vTimestampValue
  fromEntity _     = Nothing

instance DatastoreEntity LatLng where
  toEntity l = V $ value & vGeoPointValue ?~ l
  fromEntity (V v) = v^.vGeoPointValue
  fromEntity _     = Nothing

instance DatastoreEntity a => DatastoreEntity (NonEmpty a) where
  toEntity xs = V $ value & vArrayValue ?~ (arrayValue & avValues .~
                                             (toList xs >>= indiv))
    where indiv x = case toEntity x of
            V v -> [v]
            _   -> []
  fromEntity (V v) = do vals   <- (v^?vArrayValue._Just.avValues)
                        parsed <- traverse fromEntity $ V <$> vals
                        return $ fromList parsed
  fromEntity _     = Nothing

instance DatastoreEntity a => DatastoreEntity [a] where
  toEntity xs = V $ value & vArrayValue ?~ (arrayValue & avValues .~
                                             (xs >>= indiv))
    where indiv x = case toEntity x of
            V v -> [v]
            _   -> []
  fromEntity (V v) = do vals <- v^?vArrayValue._Just.avValues
                        traverse fromEntity $ V <$> vals
  fromEntity _     = Nothing

instance DatastoreEntity a => DatastoreEntity (Maybe a) where
  toEntity (Just x) = toEntity x
  toEntity Nothing  = V $ value & vNullValue ?~ NullValue
  fromEntity z@(V v) = (v^.vNullValue >> Nothing) <|> fromEntity z
  fromEntity _       = Nothing

-- instance (Monoid a, DatastoreEntity b) => DatastoreEntity (Either a b) where
--   toEntity (Right x) = toEntity x
--   toEntity (Left _)  = V $ value & vNullValue ?~ NullValue
--   -- fromEntity z@(V v) = (v^.vNullValue >> Left mempty) <|> fromEntity z -- TODO FIX MAYBE -> EITHER
--   fromEntity _     = Nothing

data Colour = Red | Green | Blue deriving (Eq, Show, Generic)

instance DatastoreEntity Colour where
  toEntity e       = V $ value & vStringValue ?~ pack (show e)
  fromEntity (V v) = case v^.vStringValue of
    Just "Red"   -> pure Red
    Just "Green" -> pure Green
    Just "Blue"  -> pure Blue
    Nothing      -> Nothing

data RecordTest
  = RecordTest
    { _foo :: Text
    , _bar :: Colour
    } deriving (Eq, Show, Generic, DatastoreEntity)

data RecordTest2
  = RecordTest2
    { _baz  :: Text
    , _quux :: RecordTest
    } deriving (Eq, Show, Generic, DatastoreEntity)

