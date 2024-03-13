module Test.Main where

import Prelude

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Lens (over, review, toArrayOf)
import Data.Lens.Spectrometer (boundedEnumSpectrometer, tupleSpectrometer)
import Data.Lens.Spectrometer.Generics (genericSpectrometer)
import Data.Lens.Spectrometer.Record (recordSpectrometer)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (Tuple2)
import Effect (Effect)
import Effect.Class.Console (logShow)

main :: Effect Unit
main = do
  let
    superTuple :: Tuple2 Int Int
    superTuple = review tupleSpectrometer 1

  logShow superTuple -- (Tuple 1 (Tuple 1 unit))

  let
    testType :: TestType
    testType = review (genericSpectrometer @TestType @TestType) 1

  logShow testType -- (TestType 1 1 1 1)

  let
    testRecord :: { a :: Int, b :: Int, c :: Int, d :: Int }
    testRecord = review recordSpectrometer 1

    testRecord2 :: { a :: Int, b :: Int, c :: Int, d :: Int }
    testRecord2 = over recordSpectrometer (_ * 2) testRecord

  logShow testRecord -- { a: 1, b: 1, c: 1, d: 1 }
  logShow testRecord2 -- { a: 2, b: 2, c: 2, d: 2 }

  let
    price :: Item -> Int
    price Pen = 10
    price Pencil = 5
    price Paper = 20

    newPrice :: Item -> Int
    newPrice = over boundedEnumSpectrometer (_ * 2) price

    newPrices :: Array Int
    newPrices = toArrayOf boundedEnumSpectrometer newPrice

  logShow newPrices -- [20,10,40]

-- | Generics Test
data TestType = TestType Int Int Int Int

derive instance Generic TestType _

instance Show TestType where
  show = genericShow

data Item = Pen | Pencil | Paper

derive instance Generic Item _
derive instance Eq Item
derive instance Ord Item

instance Show Item where
  show = genericShow

instance Enum Item where
  succ = genericSucc
  pred = genericPred

instance Bounded Item where
  bottom = genericBottom
  top = genericTop

instance BoundedEnum Item where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
