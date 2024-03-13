module Data.Lens.Spectrometer.Internal.Thicken where

import Prelude

import Data.Enum (class BoundedEnum)
import Data.Profunctor (class Profunctor)

-- | In Haskell, this class should be defined as:
-- | ```haskell
-- | thicken :: forall a b. p a b -> p (Stream a) (Stream b)
-- | ```
-- | However, PureScript evaluates expressions strictly, so infinite `Stream` cannot be Traversable
-- | Instead, use `BoundedEnum i => i -> a` to indicate _finite_ data structure
class Profunctor p <= Thicken p where
  thicken :: forall i f a b. BoundedEnum i => p a b -> p (i -> a) (i -> b)

-- thickenFromClosed :: forall p f a i b. Closed p => p a b -> p (f a) (f b)
-- thickenFromClosed = closed

-- thickenFromWander :: forall p a f b. Wander p => Traversable f => p a b -> p (f a) (f b)
-- thickenFromWander = wander traverse

-- instance Thicken Function where
--   thicken = map

-- instance Applicative f => Thicken (Star f) where
--   thicken = thickenFromWander

-- -- | For Review
-- instance Thicken Tagged where
--   thicken = thickenFromClosed

-- -- | For Fold
-- instance Monoid r => Thicken (Forget r) where
--   thicken = thickenFromWander

-- -- | For Zipper
-- instance Functor f => Thicken (Costar f) where
--   thicken = thickenFromClosed

-- -- | For Zipping
-- instance Thicken Zipping where
--   thicken = thickenFromClosed
