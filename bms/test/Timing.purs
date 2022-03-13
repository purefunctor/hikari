module Test.Timing where

import Prelude

import BMS.Timing (measureTimings, noteTimings)
import BMS.Types (Measure(..), Note(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

bpm :: Number
bpm = 120.0

start :: Number
start = 0.0

notes :: NonEmpty List (NonEmpty List Note)
notes = (note :| note : note : note : Nil) :| Nil

notes' :: NonEmpty List (NonEmpty List Note)
notes' = (note :| note : note : note : Nil) :| (note' :| note' : note' : note' : Nil) : Nil

note :: Note
note = Note "00"

note' :: Note
note' = Note "01"

measure :: Measure
measure = Measure { factor: 1.0, index: 0, notes }

measure' :: Measure
measure' = Measure { factor: 1.0, index: 0, notes: notes' }

testTiming :: Spec Unit
testTiming = do
  describe "noteTimings" do
    it "should divide a measure equally" do
      noteTimings bpm start measure `shouldEqual`
        (Map.fromFoldable $ (_ /\ (note :| Nil)) <$> [ 0.0, 0.5, 1.0, 1.5 ])
    it "should merge chords together" do
      noteTimings bpm start measure' `shouldEqual`
        (Map.fromFoldable $ (_ /\ (note :| note' : Nil)) <$> [ 0.0, 0.5, 1.0, 1.5 ])
  describe "measureTimings" do
    it "should advance proportionally" do
      let measures =
            (0 :| 1 : 2 : 3 : Nil) <#> \index -> Measure { factor: 1.0, index, notes }
      Map.values (measureTimings bpm measures) `shouldEqual` List.fromFoldable [0.0, 2.0, 4.0, 6.0]
    it "should advance given the factor" do
      let measures =
            Measure { factor: 2.0, index: 0, notes } :| ((1 : 2 : 3 : Nil) <#> \index -> Measure { factor: 1.0, index, notes })
      Map.values (measureTimings bpm measures) `shouldEqual` List.fromFoldable [0.0, 4.0, 6.0, 8.0]
