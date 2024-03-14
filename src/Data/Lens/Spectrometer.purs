module Data.Lens.Spectrometer where

import Prelude

import Data.Enum (class BoundedEnum)
import Data.Lens (Optic, iso, re)
import Data.Lens.Spectrometer.Internal.Cable (class CableToTuple, Cable(..), cableTupleIso)
import Data.Lens.Spectrometer.Internal.Representable (class Representable)
import Data.Lens.Spectrometer.Internal.Thicken (class Thicken, thicken)
import Data.Traversable (class Traversable)
import Safe.Coerce (coerce)

-- | Between Iso and Grate
-- | and between Iso and Traversal !
type Spectrometer s t a b = forall p. Thicken p => Optic p s t a b

type Spectrometer' s a = Spectrometer s s a a

spectrometer
  :: forall i f a b
   . Traversable f
  => Representable i f
  => Spectrometer (f a) (f b) a b
spectrometer = thicken

tupleSpectrometer
  :: forall size a b tupleA tupleB
   . CableToTuple (Cable size a) tupleA
  => CableToTuple (Cable size b) tupleB
  => BoundedEnum size
  => Spectrometer tupleA tupleB a b
tupleSpectrometer = re cableTupleIso <<< spectrometer

boundedEnumSpectrometer
  :: forall key a b
   . BoundedEnum key
  => Spectrometer (key -> a) (key -> b) a b
boundedEnumSpectrometer = iso Cable coerce <<< spectrometer
