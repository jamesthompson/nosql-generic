{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Gogol.DatastoreEntity where

import           Control.Lens             hiding (from, to)
import qualified Data.HashMap.Lazy        as HM
import           Data.Text                (Text, pack)
import           GHC.Generics
import           Network.Google.Datastore (Value, eProperties, entity,
                                           entityProperties, vEntityValue,
                                           value)

data EntityTransform
  = Entity Text [(Text, Value)]
  | EntityP [(Text, Value)]
  | V Value
  | None
  deriving (Eq, Show)

class DatastoreEntity a where
  entity' :: a -> EntityTransform
  default entity' :: (Generic a, GDatastore (Rep a)) => a -> EntityTransform
  entity' = gentity' . from

class GDatastore f where
  gentity' :: f a -> EntityTransform

instance GDatastore U1 where
  gentity' _ = None

instance (GDatastore a, GDatastore b) => GDatastore (a :+: b) where
  gentity' (L1 x) = gentity' x
  gentity' (R1 x) = gentity' x

instance (GDatastore a, GDatastore b) => GDatastore (a :*: b) where
  gentity' (a :*: b) = case (gentity' a, gentity' b) of
    (EntityP xs, EntityP ys)  -> EntityP $ xs ++ ys
    _                         -> None

instance GDatastore f => GDatastore (M1 D d f) where
  gentity' = gentity' . unM1

instance (GDatastore f, Constructor c) => GDatastore (M1 C c f) where
  gentity' x
    | conIsRecord x = case gentity' $ unM1 x of
        EntityP xs -> Entity (pack (conName x)) xs
        v          -> v
    | otherwise = gentity' $ unM1 x

instance (GDatastore f, Selector c) => GDatastore (M1 S c f) where
  gentity' s@(M1 x) = case gentity' x of
      V v         -> EntityP [(pack (selName s), v)]
      EntityP xs  -> EntityP $ over _1 (const (pack (selName s))) <$> xs
      Entity _ xs -> EntityP [(pack (selName s), mkEntityV xs)]
      _           -> None
    where mkEntityV xs = value & vEntityValue ?~
                           (entity & eProperties ?~
                              entityProperties (HM.fromList xs))

instance DatastoreEntity a => GDatastore (K1 i a) where
  gentity' = entity' . unK1

