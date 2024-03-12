module Test.Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (review)
import Data.Lens.Spectrometer (Spectrometer, Spectrometer', spectrometer)
import Data.Lens.Spectrometer.AutoGen.Generics (genericSpectrometer)
import Data.Lens.Spectrometer.AutoGen.Record (recordSpectrometer)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (Tuple5)
import Effect (Effect)
import Effect.Class.Console (logShow)

main :: Effect Unit
main = do
  logShow superTuple
  logShow testType
  logShow testRecord

type SuperTuple a = Tuple5 a a a a a

superSpectrometer :: forall a b. Spectrometer (SuperTuple a) (SuperTuple b) a b
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

-- | Record Test
type TestRecord = { a :: Int, b :: Int, c :: Int, d :: Int }

testRecordSpectrometer :: Spectrometer' TestRecord Int
testRecordSpectrometer = recordSpectrometer

testRecord :: { a :: Int, b :: Int, c :: Int, d :: Int }
testRecord = review testRecordSpectrometer 1
