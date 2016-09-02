{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Gogol.DatastoreEntity where

import           Control.Applicative      ((<|>))
import           Control.Lens             hiding (from, to)
import qualified Data.HashMap.Lazy        as HM
import           Data.Text                (Text, pack)
import           GHC.Generics
import           Network.Google.Datastore (Entity, EntityProperties, Value,
                                           eProperties, entity,
                                           entityProperties, epAddtional,
                                           vEntityValue, value)

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

instance DatastoreEntity a => GDatastorePut (K1 i a) where
  gEntPut = toEntity . unK1

instance DatastoreEntity a => GDatastoreGet (K1 i a) where
  gEntGet = fmap K1 . fromEntity

