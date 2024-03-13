module Data.Lens.Spectrometer.Generics where

import Prelude

import Data.Enum (class BoundedEnum)
import Data.Generic.Rep as G
import Data.Lens (Iso, iso, withIso)
import Data.Lens.Spectrometer (Spectrometer, spectrometer)
import Data.Lens.Spectrometer.Internal.Cable (Cable, cons, head, nil, tail)
import Data.Lens.Spectrometer.Internal.Idx (Idx)

class GenericSpectrometer :: Type -> Type -> Type -> Type -> Type -> Constraint
class GenericSpectrometer typeA typeB size a b | typeA -> size a, typeB -> size b where
  genericSpectrometer :: Spectrometer typeA typeB a b

instance (TypeCableIso size a b typeA typeB, BoundedEnum size) => GenericSpectrometer typeA typeB size a b where
  genericSpectrometer = typeCableIso <<< spectrometer

-- | Type と Cable の Iso
class TypeCableIso size a b typeA typeB | typeA -> size a, typeB -> size b where
  typeCableIso :: Iso typeA typeB (Cable size a) (Cable size b)

instance (RepCableIso size a b repA repB, G.Generic typeA repA, G.Generic typeB repB) => TypeCableIso size a b typeA typeB where
  typeCableIso = iso G.from G.to <<< repCableIso

-- | Rep と Cable の Iso
class RepCableIso size a b repA repB | size a -> repA, repA -> size a, size b -> repB, repB -> size b where
  repCableIso :: Iso repA repB (Cable size a) (Cable size b)

instance ArgumentCableIso size a b argA argB => RepCableIso size a b (G.Constructor sym argA) (G.Constructor sym argB) where
  repCableIso = constructorIso <<< argumentCableIso
    where
    constructorIso :: Iso (G.Constructor sym argA) (G.Constructor sym argB) argA argB
    constructorIso = iso (\(G.Constructor arg) -> arg) G.Constructor

-- | Argument と Cable の Iso
class ArgumentCableIso size a b argA argB | size a -> argA, argA -> size a, size b -> argB, argB -> size b where
  argumentCableIso :: Iso argA argB (Cable size a) (Cable size b)

instance ArgumentCableIso Void a b G.NoArguments G.NoArguments where
  argumentCableIso = iso (\_ -> nil) (\_ -> G.NoArguments)

instance ArgumentCableIso (Idx Void) a b (G.Argument a) (G.Argument b) where
  argumentCableIso = iso (\(G.Argument a) -> a `cons` nil) (\cable -> G.Argument $ head cable)

else instance ArgumentCableIso size a b argA argB => ArgumentCableIso (Idx size) a b (G.Product (G.Argument a) argA) (G.Product (G.Argument b) argB) where
  argumentCableIso = withIso argumentCableIso \argToCableA cableToArgB -> iso (\(G.Product (G.Argument a) argA) -> a `cons` argToCableA argA) (\cable -> G.Product (G.Argument $ head cable) $ cableToArgB $ tail cable)
