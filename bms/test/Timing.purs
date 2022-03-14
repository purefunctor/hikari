module Test.Timing where

import Prelude

import BMS.Timing (measureTimings, noteTimings, scoreTimings)
import BMS.Types (Measure(..), Note(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.NonEmpty (NonEmpty, (:|))
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

bpm :: Number
bpm = 120.0

start :: Number
start = 0.0

columns :: NonEmpty List (NonEmpty List Note)
columns = (note :| note : note : note : Nil) :| Nil

columns' :: NonEmpty List (NonEmpty List Note)
columns' = (note :| note : note : note : Nil) :| (note' :| note' : note' : note' : Nil) : Nil

note :: Note
note = Note "00"

note' :: Note
note' = Note "01"

measure :: Measure
measure = Measure { factor: 1.0, index: 0, columns }

measure' :: Measure
measure' = Measure { factor: 1.0, index: 0, columns: columns' }

testTiming :: Spec Unit
testTiming = describe "BMS.Timing" do
  describe "noteTimings" do
    it "should divide a measure equally" do
      noteTimings bpm start measure `shouldEqual`
        (Map.fromFoldable $ (_ /\ (note :| Nil)) <$> [ 0.0, 0.5, 1.0, 1.5 ])
    it "should merge chords together" do
      noteTimings bpm start measure' `shouldEqual`
        (Map.fromFoldable $ (_ /\ (note :| note' : Nil)) <$> [ 0.0, 0.5, 1.0, 1.5 ])
  describe "measureTimings" do
    it "should advance proportionally" do
      let
        measures =
          (0 :| 1 : 2 : 3 : Nil) <#> \index -> Measure { factor: 1.0, index, columns }
      Map.values (measureTimings bpm measures) `shouldEqual` List.fromFoldable
        [ 0.0, 2.0, 4.0, 6.0 ]
    it "should advance given the factor" do
      let
        measures =
          Measure { factor: 2.0, index: 0, columns } :|
            ((1 : 2 : 3 : Nil) <#> \index -> Measure { factor: 1.0, index, columns })
      Map.values (measureTimings bpm measures) `shouldEqual` List.fromFoldable
        [ 0.0, 4.0, 6.0, 8.0 ]
  describe "scoreTimings" do
    it "should compose measureTimings and noteTimings" do
      let
        measures =
          (0 :| 1 : 2 : 3 : Nil) <#> \index -> Measure { factor: 1.0, index, columns }
      Map.keys (scoreTimings bpm measures) `shouldEqual` Set.fromFoldable
        [ 0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0, 7.5 ]
