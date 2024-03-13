module Test.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (review, sequenceOf)
import Data.Lens.Spectrometer (Spectrometer', spectrometer)
import Data.Lens.Spectrometer.Generics (genericSpectrometer)
import Data.Lens.Spectrometer.Record (recordSpectrometer)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (Tuple5)
import Effect (Effect)
import Effect.Class.Console (logShow)

main :: Effect Unit
main = do
  logShow superTuple
  logShow testType
  logShow testRecord
  void $ logs

type SuperTuple a = Tuple5 a a a a a

superSpectrometer :: forall a. Spectrometer' (SuperTuple a) a
superSpectrometer = spectrometer

superTuple :: Tuple5 Int Int Int Int Int
superTuple = review superSpectrometer 1

-- | Generics Test
data TestType = TestType Int Int Int Int

derive instance Generic TestType _

instance Show TestType where
  show = genericShow

testTypeSpectrometer :: Spectrometer' TestType Int
testTypeSpectrometer = genericSpectrometer

testType :: TestType
testType = review testTypeSpectrometer 1

testRecordSpectrometer :: Spectrometer' { a :: Int, b :: Int, c :: Int, d :: Int } Int
testRecordSpectrometer = recordSpectrometer

testRecord :: { a :: Int, b :: Int, c :: Int, d :: Int }
testRecord = review testRecordSpectrometer 1

record = { a: logShow 1, b: logShow 2 }

logs = sequenceOf recordSpectrometer record
