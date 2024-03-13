module Data.Lens.Spectrometer.Internal.Idx where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), cardinality, fromEnum, pred, succ, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

-- | Void ...
-- | Idx Void ... 0
-- | Idx (Idx Void)  ... 0, 1
-- | ...
data Idx a = Zero | Succ a

derive instance Generic (Idx a) _
instance Show a => Show (Idx a) where
  show = genericShow

derive instance Eq a => Eq (Idx a)
derive instance Ord a => Ord (Idx a)

instance Enum (Idx Void) where
  succ _ = Nothing
  pred _ = Nothing

else instance BoundedEnum a => Enum (Idx a) where
  succ a = Succ <$> case a of
    Zero -> Just bottom
    Succ i -> succ i
  pred = case _ of
    Zero -> Nothing
    Succ i -> Just case pred i of
      Just j -> Succ j
      Nothing -> Zero

instance Bounded (Idx Void) where
  bottom = Zero
  top = Zero

else instance BoundedEnum a => Bounded (Idx a) where
  bottom = Zero
  top = Succ top

instance BoundedEnum (Idx Void) where
  cardinality = Cardinality 1
  toEnum i = if i == 0 then Just Zero else Nothing
  fromEnum = case _ of
    Zero -> 0
    Succ v -> absurd v

else instance (BoundedEnum a, Bounded (Idx a), Enum (Idx a)) => BoundedEnum (Idx a) where
  cardinality = case cardinality @a of
    Cardinality n -> Cardinality $ n + 1
  toEnum i = if i == 0 then Just Zero else Succ <$> toEnum i
  fromEnum = case _ of
    Zero -> 0
    Succ v -> 1 + fromEnum v
