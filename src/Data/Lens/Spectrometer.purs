module Data.Lens.Spectrometer where

import Prelude

import Data.Lens (Optic, re)
import Data.Lens.Spectrometer.Internal.Cable (class CableToTuple, Cable, cableTupleIso)
import Data.Lens.Spectrometer.Internal.Representable (class Representable)
import Data.Lens.Spectrometer.Internal.Thicken (class Thicken, thicken)
import Data.Traversable (class Traversable)

-- | Between Iso and Grate
-- | and between Iso and Traversal !
type Spectrometer s t a b = forall p. Thicken p => Optic p s t a b

type Spectrometer' s a = Spectrometer s s a a

represented
  :: forall i f a b
   . Traversable f
  => Representable i f
  => Spectrometer (f a) (f b) a b
represented = thicken

spectrometer
  :: forall thickness a b tupleA tupleB
   . CableToTuple (Cable thickness a) tupleA
  => CableToTuple (Cable thickness b) tupleB
  => Traversable (Cable thickness)
  => Spectrometer tupleA tupleB a b
spectrometer = re cableTupleIso <<< represented
