module Data.Lens.Spectrometer.Internal.Cable where

import Prelude

import Data.Distributive (class Distributive)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Lens (Iso, iso)
import Data.Lens.Spectrometer.Internal.Representable (class Representable)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple (Tuple(..))

-- | Void ...
-- | Next Void ... 0
-- | Next (Next Void)  ... 0, 1
-- | ...
data Next a = Zero | Next a

-- | i <- Void, Next Void, Next (Next Void) ...
newtype Cable i a = Cable (i -> a)

derive newtype instance Functor (Cable i)
derive newtype instance Distributive (Cable i)
derive newtype instance Representable i (Cable i)
derive newtype instance Apply (Cable i)
derive newtype instance Applicative (Cable i)
derive newtype instance Bind (Cable i)
derive newtype instance Monad (Cable i)

nil :: forall a. Cable Void a
nil = Cable absurd

uncons :: forall i a. Cable (Next i) a -> Tuple a (Cable i a)
uncons (Cable g) = Tuple (g Zero) $ Cable \i -> g (Next i)

cons :: forall i a. a -> Cable i a -> Cable (Next i) a
cons h (Cable t) = Cable \i -> case i of
  Zero -> h
  Next j -> t j

-- | 長さ = 0
instance Foldable (Cable Void) where
  foldMap _ _ = mempty
  foldr _ b _ = b
  foldl _ b _ = b

-- | 長さ > 0
instance Foldable (Cable i) => Foldable (Cable (Next i)) where
  foldMap f cable = f h <> foldMap f t
    where
    Tuple h t = uncons cable
  foldr f b cable = f h $ foldr f b t
    where
    Tuple h t = uncons cable
  foldl f b cable = foldl f (f b h) t
    where
    Tuple h t = uncons cable

instance Traversable (Cable Void) where
  traverse _ _ = pure nil
  sequence _ = pure nil

instance Traversable (Cable i) => Traversable (Cable (Next i)) where
  traverse f cable = cons <$> f h <*> traverse f t
    where
    Tuple h t = uncons cable
  sequence cable = cons <$> h <*> sequence t
    where
    Tuple h t = uncons cable

class CableToTuple cable tuple | cable -> tuple, tuple -> cable where
  toTuple :: cable -> tuple
  fromTuple :: tuple -> cable

instance CableToTuple (Cable Void a) Unit where
  toTuple _ = unit
  fromTuple _ = nil

instance CableToTuple (Cable i a) tuple => CableToTuple (Cable (Next i) a) (Tuple a tuple) where
  toTuple cable = Tuple h $ toTuple t
    where
    Tuple h t = uncons cable
  fromTuple (Tuple h t) = cons h $ fromTuple t

cableTupleIso :: forall cable1 cable2 tuple1 tuple2. CableToTuple cable1 tuple1 => CableToTuple cable2 tuple2 => Iso cable1 cable2 tuple1 tuple2
cableTupleIso = iso toTuple fromTuple
