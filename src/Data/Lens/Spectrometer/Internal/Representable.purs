module Data.Lens.Spectrometer.Internal.Representable where

import Prelude

import Control.Monad.Reader (ReaderT(..))
import Data.Distributive (class Distributive)
import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..))

class Distributive f <= Representable i f | f -> i where
  tabulate :: forall a. (i -> a) -> f a
  index :: forall a. f a -> i -> a

instance Representable Unit Identity where
  tabulate rep = Identity (rep unit)
  index (Identity a) _ = a

instance Representable r (Function r) where
  tabulate = identity
  index = identity

instance Representable i m => Representable (Tuple i r) (ReaderT r m) where
  tabulate rep = ReaderT \r -> tabulate \i -> rep $ Tuple i r
  index (ReaderT f) (Tuple i r) = index (f r) i
