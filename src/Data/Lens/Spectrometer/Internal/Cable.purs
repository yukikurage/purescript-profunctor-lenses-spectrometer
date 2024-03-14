module Data.Lens.Spectrometer.Internal.Cable where

import Prelude

import Data.Distributive (class Distributive)
import Data.Enum (class BoundedEnum, enumFromTo)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Lens (Iso, iso)
import Data.Lens.Spectrometer.Internal.Aligned (class Aligned)
import Data.Lens.Spectrometer.Internal.Idx (Idx(..))
import Data.Lens.Spectrometer.Internal.Representable (class Representable)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)

-- | Wrapper for a function, becoming `Traversable` when the index type is `BoundedEnum`.
-- | Cable usually uses `Idx` as the index type.
newtype Cable i a = Cable (i -> a)

derive newtype instance Functor (Cable i)
derive newtype instance Distributive (Cable i)
derive newtype instance Representable i (Cable i)
derive newtype instance Apply (Cable i)
derive newtype instance Applicative (Cable i)
derive newtype instance Bind (Cable i)
derive newtype instance Monad (Cable i)

toArray :: forall i a. BoundedEnum i => Cable i a -> Array a
toArray (Cable g) = map g fullEnum

fullEnum :: forall @a. BoundedEnum a => Array a
fullEnum = enumFromTo bottom top

-- | 長さ = 0
instance BoundedEnum a => Foldable (Cable a) where
  foldMap f cable = foldMap f $ toArray cable
  foldr f b cable = foldr f b $ toArray cable
  foldl f b cable = foldl f b $ toArray cable

instance BoundedEnum a => Traversable (Cable a) where
  traverse mapping (Cable dict) = foldr f (pure unsafe) fullEnum
    where
    unsafe :: forall i x. Cable i x
    unsafe = Cable \_ -> unsafeCrashWith "Data.Lens.Spectrometer.Internal.Cable.traverse: impossible"
    f i accA = (\iElem (Cable acc) -> Cable \j -> if j == i then iElem else acc j) <$> mapping (dict i) <*> accA
  sequence (Cable dict) = foldr f (pure unsafe) fullEnum
    where
    unsafe :: forall i x. Cable i x
    unsafe = Cable \_ -> unsafeCrashWith "Data.Lens.Spectrometer.Internal.Cable.sequence: impossible"
    f i accA = (\iElem (Cable acc) -> Cable \j -> if j == i then iElem else acc j) <$> dict i <*> accA

instance BoundedEnum i => Aligned i (Cable i)

nil :: forall a. Cable Void a
nil = Cable absurd

cons :: forall i a. a -> Cable i a -> Cable (Idx i) a
cons a (Cable f) = Cable \i -> case i of
  Zero -> a
  Succ j -> f j

head :: forall i a. Cable (Idx i) a -> a
head (Cable f) = f Zero

tail :: forall i a. Cable (Idx i) a -> Cable i a
tail (Cable f) = Cable \i -> f (Succ i)

class CableToTuple cable tuple | cable -> tuple, tuple -> cable where
  toTuple :: cable -> tuple
  fromTuple :: tuple -> cable

instance CableToTuple (Cable Void a) Unit where
  toTuple _ = unit
  fromTuple _ = nil

instance CableToTuple (Cable i a) tuple => CableToTuple (Cable (Idx i) a) (Tuple a tuple) where
  toTuple cable = Tuple (head cable) $ toTuple (tail cable)
  fromTuple (Tuple h t) = cons h $ fromTuple t

cableTupleIso :: forall cable1 cable2 tuple1 tuple2. CableToTuple cable1 tuple1 => CableToTuple cable2 tuple2 => Iso cable1 cable2 tuple1 tuple2
cableTupleIso = iso toTuple fromTuple
