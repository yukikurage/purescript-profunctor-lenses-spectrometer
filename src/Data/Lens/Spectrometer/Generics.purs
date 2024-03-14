module Data.Lens.Spectrometer.Generics where

import Prelude

import Data.Enum (class BoundedEnum)
import Data.Generic.Rep as G
import Data.Lens (Iso, iso, withIso)
import Data.Lens.Spectrometer (Spectrometer, spectrometer)
import Data.Lens.Spectrometer.Internal.Cable (Cable, cons, head, nil, tail)
import Data.Lens.Spectrometer.Internal.Idx (Idx)

class GenericSpectrometer :: Type -> Type -> Type -> Type -> Constraint
class GenericSpectrometer typeA typeB a b | typeA -> a, typeB -> b where
  genericSpectrometer :: Spectrometer typeA typeB a b

instance (TypeCableIso idx a b typeA typeB, BoundedEnum idx) => GenericSpectrometer typeA typeB a b where
  genericSpectrometer = typeCableIso <<< spectrometer

-- | Type と Cable の Iso
class TypeCableIso idx a b typeA typeB | typeA -> idx a, typeB -> idx b where
  typeCableIso :: Iso typeA typeB (Cable idx a) (Cable idx b)

instance (RepCableIso idx a b repA repB, G.Generic typeA repA, G.Generic typeB repB) => TypeCableIso idx a b typeA typeB where
  typeCableIso = iso G.from G.to <<< repCableIso

-- | Rep と Cable の Iso
class RepCableIso idx a b repA repB | idx a -> repA, repA -> idx a, idx b -> repB, repB -> idx b where
  repCableIso :: Iso repA repB (Cable idx a) (Cable idx b)

instance ArgumentCableIso idx a b argA argB => RepCableIso idx a b (G.Constructor sym argA) (G.Constructor sym argB) where
  repCableIso = constructorIso <<< argumentCableIso
    where
    constructorIso :: Iso (G.Constructor sym argA) (G.Constructor sym argB) argA argB
    constructorIso = iso (\(G.Constructor arg) -> arg) G.Constructor

-- | Argument と Cable の Iso
class ArgumentCableIso idx a b argA argB | idx a -> argA, argA -> idx a, idx b -> argB, argB -> idx b where
  argumentCableIso :: Iso argA argB (Cable idx a) (Cable idx b)

instance ArgumentCableIso Void a b G.NoArguments G.NoArguments where
  argumentCableIso = iso (\_ -> nil) (\_ -> G.NoArguments)

instance ArgumentCableIso (Idx Void) a b (G.Argument a) (G.Argument b) where
  argumentCableIso = iso (\(G.Argument a) -> a `cons` nil) (\cable -> G.Argument $ head cable)

else instance ArgumentCableIso idx a b argA argB => ArgumentCableIso (Idx idx) a b (G.Product (G.Argument a) argA) (G.Product (G.Argument b) argB) where
  argumentCableIso = withIso argumentCableIso \argToCableA cableToArgB -> iso (\(G.Product (G.Argument a) argA) -> a `cons` argToCableA argA) (\cable -> G.Product (G.Argument $ head cable) $ cableToArgB $ tail cable)
