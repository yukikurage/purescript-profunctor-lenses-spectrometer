module Data.Lens.Spectrometer.Internal.Thicken where

import Prelude

import Data.Functor.Costar (Costar)
import Data.Lens (class Wander, Forget, Tagged, wander)
import Data.Lens.Internal.Zipping (Zipping)
import Data.Lens.Spectrometer.Internal.Representable (class Representable, index, tabulate)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Closed (class Closed, closed)
import Data.Profunctor.Star (Star)
import Data.Traversable (class Traversable, traverse)

-- | In Haskell, this class should be defined as:
-- | ```haskell
-- | thicken :: forall a b. p a b -> p (Stream a) (Stream b)
-- | ```
-- | However, PureScript evaluates expressions strictly, so infinite `Stream` cannot be Traversable
-- | Instead, use `Traversable f, Representable i f` to indicate _finite_ data structure
class Profunctor p <= Thicken p where
  thicken :: forall i f a b. Traversable f => Representable i f => p a b -> p (f a) (f b)

thickenFromClosed :: forall p f a i b. Closed p => Representable i f => p a b -> p (f a) (f b)
thickenFromClosed = dimap index tabulate <<< closed

thickenFromWander :: forall p a f b. Wander p => Traversable f => p a b -> p (f a) (f b)
thickenFromWander = wander traverse

instance Thicken Function where
  thicken = map

instance Applicative f => Thicken (Star f) where
  thicken = thickenFromWander

-- | For Review
instance Thicken Tagged where
  thicken = thickenFromClosed

-- | For Fold
instance Monoid r => Thicken (Forget r) where
  thicken = thickenFromWander

-- | For Zipper
instance Functor f => Thicken (Costar f) where
  thicken = thickenFromClosed

-- | For Zipping
instance Thicken Zipping where
  thicken = thickenFromClosed
