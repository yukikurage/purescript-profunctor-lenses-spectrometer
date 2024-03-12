module Data.Lens.Spectrometer.Record where

import Prelude

import Data.Lens (Iso, iso, withIso)
import Data.Lens.Spectrometer (Spectrometer, spectrometer)
import Data.Lens.Spectrometer.Internal.Cable (class CableToTuple, Cable)
import Data.Symbol (class IsSymbol)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
import Prim.Row as R
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record (delete, get, insert)
import Type.Proxy (Proxy(..))

class RecordSpectrometer :: Row Type -> Row Type -> Type -> Type -> Constraint
class RecordSpectrometer s t a b | s -> a, t -> b where
  recordSpectrometer :: Spectrometer (Record s) (Record t) a b

data UnitK

foreign import data UnitT :: UnitK

class FillRecord :: RowList UnitK -> Type -> RowList Type -> Constraint
class FillRecord rl a r | rl a -> r, r -> rl a

instance FillRecord RL.Nil a RL.Nil
instance (FillRecord rl a rl') => FillRecord (RL.Cons sym UnitT rl) a (RL.Cons sym a rl')

instance
  ( FillRecord rl a rlS
  , FillRecord rl b rlT
  , RowToList s rlS
  , RowToList t rlT
  , RecordIso rlS rlT s t tupleA tupleB
  , CableToTuple (Cable thickness a) tupleA
  , CableToTuple (Cable thickness b) tupleB
  , Traversable (Cable thickness)
  ) =>
  RecordSpectrometer s t a b
  where
  recordSpectrometer = recordIso (Proxy :: Proxy rlS) (Proxy :: Proxy rlT) <<< spectrometer

class RecordIso :: RL.RowList Type -> RL.RowList Type -> Row Type -> Row Type -> Type -> Type -> Constraint
class RecordIso rlS rlT s t a b | rlS -> s a, rlT -> t b where
  recordIso :: Proxy rlS -> Proxy rlT -> Iso (Record s) (Record t) a b

instance RecordIso RL.Nil RL.Nil () () Unit Unit where
  recordIso _ _ = iso (\_ -> unit) (\_ -> {})

instance
  ( RecordIso rlS rlT tailS tailT tailA tailB
  , R.Cons sym a tailS s
  , R.Lacks sym tailS
  , R.Cons sym b tailT t
  , R.Lacks sym tailT
  , IsSymbol sym
  ) =>
  RecordIso (RL.Cons sym a rlS) (RL.Cons sym b rlT) s t (Tuple a tailA) (Tuple b tailB) where
  recordIso _ _ = iso (\rec -> Tuple (get sym rec) (tailSToTailA $ delete sym rec)) (\(Tuple b tailB) -> insert sym b $ tailBToTailT tailB)
    where
    sym = Proxy :: _ sym
    Tuple (tailSToTailA :: Record tailS -> tailA) (tailBToTailT :: tailB -> Record tailT) = withIso (recordIso (Proxy :: Proxy rlS) (Proxy :: Proxy rlT)) Tuple
