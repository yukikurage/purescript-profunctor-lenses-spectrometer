module Data.Lens.Spectrometer.AutoGen.Generics where

import Prelude

import Data.Generic.Rep as G
import Data.Lens (Iso, iso, withIso)
import Data.Lens.Spectrometer (Spectrometer, spectrometer)
import Data.Lens.Spectrometer.Internal.Cable (class CableToTuple, Cable)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))

class GenericSpectrometer :: Type -> Type -> Type -> Type -> Constraint
class GenericSpectrometer s t a b | s -> a, t -> b where
  genericSpectrometer :: Spectrometer s t a b

-- | Rep 用
class GenericSpectrometerRep s t a b | s -> a, t -> b where
  genericSpectrometerRep :: Spectrometer s t a b

-- | Argument 用
-- | Argument と Tuple の Iso
class ArgumentIso s t a b | s -> a, t -> b where
  argumentIso :: Iso s t a b

instance
  ( G.Generic s repS
  , G.Generic t repT
  , GenericSpectrometerRep repS repT a b
  ) =>
  GenericSpectrometer s t a b where
  genericSpectrometer = iso G.from G.to <<< genericSpectrometerRep

-- -- | Constructor 1 つにしか対応しないので Product は考えない
instance
  ( ArgumentIso s t tupleA tupleB
  , CableToTuple (Cable thickness a) tupleA
  , CableToTuple (Cable thickness b) tupleB
  , Traversable (Cable thickness)
  ) =>
  GenericSpectrometerRep (G.Constructor symS s) (G.Constructor symT t) a b where
  genericSpectrometerRep = constructorIso <<< argumentIso <<< spectrometer
    where
    constructorIso :: Iso (G.Constructor _ s) (G.Constructor _ t) s t
    constructorIso = iso (\(G.Constructor s) -> s) G.Constructor

instance ArgumentIso G.NoArguments G.NoArguments Unit Unit where
  argumentIso = iso (\_ -> unit) (\_ -> G.NoArguments)

instance ArgumentIso (G.Argument a) (G.Argument b) (Tuple a Unit) (Tuple b Unit) where
  argumentIso = iso (\(G.Argument a) -> Tuple a unit) (\(Tuple b _) -> G.Argument b)

instance (ArgumentIso argS argT tupleA tupleB) => ArgumentIso (G.Product (G.Argument a) argS) (G.Product (G.Argument b) argT) (Tuple a tupleA) (Tuple b tupleB) where
  argumentIso = iso (\(G.Product (G.Argument a) argS) -> Tuple a $ argSToTuple argS) (\(Tuple b tupleB) -> G.Product (G.Argument b) (tupleToArgT tupleB))
    where
    Tuple (argSToTuple :: argS -> tupleA) (tupleToArgT :: tupleB -> argT) = withIso argumentIso Tuple
