{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Gogol.DatastoreEntity (
    EntityTransform(..)
  , DatastoreEntity(..)
  , _ToDatastoreEntity
  , _FromDatastoreEntity
  ) where

import           Control.Applicative      ((<|>))
import           Control.Lens             (Getter, at, over, view, (&), (.~),
                                           (?~), (^.), (^?), _1, _Just, _head)
import qualified Control.Lens.Getter      as L
import           Data.ByteString          (ByteString)
import qualified Data.HashMap.Lazy        as HM
import           Data.Int                 (Int32, Int64)
import           Data.List.NonEmpty       (NonEmpty, fromList, toList)
import           Data.Maybe               (fromMaybe)
import           Data.Text                (Text, pack)
import           Data.Time.Clock          (UTCTime)
import           GHC.Float                (double2Float, float2Double)
import           GHC.Generics
import           Network.Google.Datastore (Entity, Key, LatLng, Value,
                                           arrayValue, avValues, eKey,
                                           eProperties, eProperties, entity,
                                           entityProperties, epAddtional, kPath,
                                           key, pathElement, peKind,
                                           vArrayValue, vBlobValue,
                                           vBooleanValue, vDoubleValue,
                                           vEntityValue, vGeoPointValue,
                                           vIntegerValue, vKeyValue,
                                           vStringValue, vTimestampValue, value)

data EntityTransform
  = EntityC Text [(Text, Value)]
  | EntityP [(Text, Value)]
  | V Value
  | None
  deriving (Eq, Show)

class DatastoreEntity a where
  toEntity :: a -> EntityTransform
  fromEntity :: EntityTransform -> Maybe a

  default toEntity :: (Generic a, GDatastorePut (Rep a)) => a -> EntityTransform
  toEntity = gEntPut . from

  default fromEntity ::(Generic a, GDatastoreGet (Rep a)) => EntityTransform -> Maybe a
  fromEntity x = to <$> gEntGet x

class GDatastorePut f where
  gEntPut :: f a -> EntityTransform

class GDatastoreGet f where
  gEntGet :: EntityTransform -> Maybe (f a)

instance GDatastorePut U1 where
  gEntPut _ = None

instance GDatastoreGet U1 where
  gEntGet _ = Nothing

instance (GDatastorePut a, GDatastorePut b) => GDatastorePut (a :+: b) where
  gEntPut (L1 x) = gEntPut x
  gEntPut (R1 x) = gEntPut x

instance (GDatastoreGet a, GDatastoreGet b) => GDatastoreGet (a :+: b) where
  gEntGet e = L1 <$> gEntGet e <|> R1 <$> gEntGet e

instance (GDatastorePut a, GDatastorePut b) => GDatastorePut (a :*: b) where
  gEntPut (a :*: b) = case (gEntPut a, gEntPut b) of
    (EntityP xs, EntityP ys)  -> EntityP $ xs ++ ys
    _                         -> None

instance (GDatastoreGet a, GDatastoreGet b) => GDatastoreGet (a :*: b) where
  gEntGet e = (:*:) <$> gEntGet e <*> gEntGet e

instance GDatastorePut f => GDatastorePut (D1 d f) where
  gEntPut = gEntPut . unM1

instance GDatastoreGet f => GDatastoreGet (D1 d f) where
  gEntGet = fmap M1 . gEntGet

instance (GDatastorePut f, Constructor c) => GDatastorePut (C1 c f) where
  gEntPut x
    | conIsRecord x = case gEntPut $ unM1 x of
        EntityP xs -> EntityC (pack (conName x)) xs
        v          -> v
    | otherwise = gEntPut $ unM1 x

instance (GDatastoreGet f, Constructor c) => GDatastoreGet (C1 c f) where
  gEntGet (EntityC _ xs) = M1 <$> gEntGet (EntityP xs)
  gEntGet (EntityP xs)   = M1 <$> gEntGet (EntityP xs)
  gEntGet (V v)          = do props <- v^.vEntityValue
                              ep    <- props^.eProperties
                              gEntGet (EntityP . HM.toList $ ep^.epAddtional)
  gEntGet None           = Nothing

instance (GDatastorePut f, Selector c) => GDatastorePut (S1 c f) where
  gEntPut s@(M1 x) = case gEntPut x of
      V v           -> EntityP [(pack (selName s), v)]
      EntityP xs    -> EntityP $ over _1 (const (pack (selName s))) <$> xs
      EntityC _  xs -> EntityP [(pack (selName s), mkEntityV xs)]
      _             -> None
    where mkEntityV xs = value & vEntityValue ?~
                           (entity & eProperties ?~
                              entityProperties (HM.fromList xs))

instance (GDatastoreGet f, Selector c) => GDatastoreGet (S1 c f) where
  gEntGet (EntityP xs) = gEntGet =<< V <$> HM.fromList xs^.at s
    where s = pack $ selName (undefined :: t c f p)
  gEntGet (V v)        = M1 <$> gEntGet (V v)
  gEntGet _            = Nothing

instance DatastoreEntity a => GDatastorePut (K1 i a) where
  gEntPut = toEntity . unK1

instance DatastoreEntity a => GDatastoreGet (K1 i a) where
  gEntGet = fmap K1 . fromEntity

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
            V v              -> [v]
            ec@(EntityC _ _) -> [value & vEntityValue .~ toEntity' ec]
            _                -> []
  fromEntity (V v) = do av     <- v^.vArrayValue
                        parsed <- traverse fromEntity (V <$> av^.avValues)
                        return $ fromList parsed
  fromEntity _     = Nothing

instance DatastoreEntity a => DatastoreEntity [a] where
  toEntity xs = V $ value & vArrayValue ?~ (arrayValue & avValues .~
                                             (xs >>= indiv))
    where indiv x = case toEntity x of
            V v              -> [v]
            ec@(EntityC _ _) -> [value & vEntityValue .~ toEntity' ec]
            _                -> []
  fromEntity (V v) = do av <- v^.vArrayValue
                        traverse fromEntity (V <$> av^.avValues)
  fromEntity _     = Nothing

instance DatastoreEntity a => DatastoreEntity (Maybe a) where
  toEntity (Just x) = toEntity x
  toEntity Nothing  = V value
  fromEntity z@(V _) = (pure <$> fromEntity z) <|> pure Nothing
  fromEntity _       = Nothing

_ToDatastoreEntity :: DatastoreEntity a => Getter a (Maybe Entity)
_ToDatastoreEntity = L.to (toEntity' . toEntity)

toEntity' :: EntityTransform -> Maybe Entity
toEntity' (EntityC k params) =
  pure $ entity & eKey ?~ (key & kPath .~ [pathElement & peKind ?~ k])
                & eProperties ?~ entityProperties (HM.fromList params)
toEntity' _                  = Nothing

_FromDatastoreEntity :: DatastoreEntity a => Getter Entity (Maybe a)
_FromDatastoreEntity = L.to (fromEntity . toTransform)
  where toTransform e =
          fromMaybe None $ EntityC <$> (view kPath <$> e^.eKey)^._Just._head.peKind
                                   <*> (HM.toList <$> (e^?eProperties._Just.epAddtional))

