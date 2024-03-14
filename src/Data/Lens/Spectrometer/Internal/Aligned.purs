module Data.Lens.Spectrometer.Internal.Aligned where

import Data.Lens.Spectrometer.Internal.Representable (class Representable)
import Data.Pair (Pair)
import Data.Traversable (class Traversable)

class (Representable i f, Traversable f) <= Aligned i f | f -> i

instance Aligned Boolean Pair
