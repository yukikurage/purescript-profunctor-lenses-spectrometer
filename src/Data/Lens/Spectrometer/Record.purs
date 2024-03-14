module Data.Lens.Spectrometer.Record where

import Prelude

import Data.Enum (class BoundedEnum)
import Data.Lens (Iso, iso, withIso)
import Data.Lens.Spectrometer (Spectrometer, spectrometer)
import Data.Lens.Spectrometer.Internal.Cable (Cable, cons, head, nil, tail)
import Data.Lens.Spectrometer.Internal.Idx (Idx)
import Data.Symbol (class IsSymbol)
import Prim.Row as R
import Prim.RowList as RL
import Record (delete, get, insert)
import Type.Proxy (Proxy(..))

class RecordSpectrometer :: Row Type -> Row Type -> Type -> Type -> Constraint
class RecordSpectrometer rowA rowB a b | rowA -> a, rowB -> b where
  recordSpectrometer :: Spectrometer (Record rowA) (Record rowB) a b

instance
  ( RL.RowToList rowA rlA
  , RL.RowToList rowB rlB
  , RecordCableIso rlA rlB rowA rowB idx a b
  , BoundedEnum idx
  ) =>
  RecordSpectrometer rowA rowB a b
  where
  recordSpectrometer = recordCableIso @rlA @rlB <<< spectrometer

class RecordCableIso :: RL.RowList Type -> RL.RowList Type -> Row Type -> Row Type -> Type -> Type -> Type -> Constraint
class
  RecordCableIso rlA rlB rowA rowB idx a b
  | rlA -> idx a -- Infer size and a from rlA
  , rlB -> idx b -- Infer size and b from rlB
  , rlA b -> rlB -- rlB has same shape as rlA, this fundep makes type inference work better.
  , rlB a -> rlA -- rlA has same shape as rlB, this fundep makes type inference work better.
  , rlA -> rowA -- Infer rowA from RowList rlA
  , rlB -> rowB -- Infer rowB from RowList rlB
  where
  recordCableIso :: Iso (Record rowA) (Record rowB) (Cable idx a) (Cable idx b)

instance RecordCableIso RL.Nil RL.Nil () () Void a b where
  recordCableIso = iso (\_ -> nil) (\_ -> {})

instance (RecordCableIso tailRlA tailRlB tailRowA tailRowB idx a b, R.Cons sym a tailRowA rowA, R.Lacks sym tailRowA, R.Cons sym b tailRowB rowB, R.Lacks sym tailRowB, IsSymbol sym) => RecordCableIso (RL.Cons sym a tailRlA) (RL.Cons sym b tailRlB) rowA rowB (Idx idx) a b where
  recordCableIso = withIso (recordCableIso @tailRlA @tailRlB) \recordToCableA cableToRecordB ->
    iso (\rec -> get sym rec `cons` (recordToCableA $ delete sym rec)) (\cable -> insert sym (head cable) $ cableToRecordB (tail cable))
    where
    sym :: Proxy sym
    sym = Proxy
