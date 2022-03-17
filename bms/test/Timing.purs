module Test.Timing where

import Prelude

import BMS.Timing (Column(..), Factor(..), Measure(..), Measures, Note(..), Notes(..), gatherAll)
import BMS.Types (BmsLine(..))
import Data.Lens (_Just, ifoldlOf) as Lens
import Data.Lens.Indexed (itraversed) as Lens
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple.Nested (type (/\), (/\))
import Safe.Coerce (coerce)
import Safe.Coerce as Safe
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

gatherMeasuresActual :: Array BmsLine
gatherMeasuresActual =
  [ ChangeFactor { measure: 0, factor: 1.25 }
  , NoteColumn { measure: 0, channel: 1, notes: [ "01", "01" ] }
  , NoteColumn { measure: 0, channel: 1, notes: [ "01", "01" ] }
  , NoteColumn { measure: 0, channel: 1, notes: [ "01", "01" ] }
  , NoteColumn { measure: 0, channel: 1, notes: [ "01", "01" ] }
  , NoteColumn { measure: 1, channel: 1, notes: [ "01", "01" ] }
  , NoteColumn { measure: 1, channel: 1, notes: [ "01", "01" ] }
  , NoteColumn { measure: 1, channel: 1, notes: [ "01", "01" ] }
  , NoteColumn { measure: 1, channel: 1, notes: [ "01", "01" ] }
  , NoteColumn { measure: 2, channel: 11, notes: [ "01", "01" ] }
  , NoteColumn { measure: 2, channel: 13, notes: [ "01", "01" ] }
  , NoteColumn { measure: 2, channel: 15, notes: [ "01", "01" ] }
  , NoteColumn { measure: 2, channel: 19, notes: [ "01", "01" ] }
  , NoteColumn { measure: 3, channel: 1, notes: [ "01", "01" ] }
  , NoteColumn { measure: 3, channel: 1, notes: [ "01", "01" ] }
  , NoteColumn { measure: 4, channel: 11, notes: [ "01", "01" ] }
  , NoteColumn { measure: 5, channel: 1, notes: [ "01", "01" ] }
  , NoteColumn { measure: 5, channel: 1, notes: [ "01", "01" ] }
  , NoteColumn { measure: 6, channel: 1, notes: [ "00" ] }
  , NoteColumn { measure: 6, channel: 1, notes: [ "00", "00" ] }
  , NoteColumn { measure: 6, channel: 1, notes: [ "01", "01", "01" ] }
  ]

mapFromJusts :: forall k v f. TraversableWithIndex k f => Ord k => f (Maybe v) -> Map k v
mapFromJusts = Lens.ifoldlOf (Lens.itraversed <<< Lens._Just) (\k a v -> Map.insert k v a) Map.empty

gatherMeasuresExpected :: Measures
gatherMeasuresExpected = Map.fromFoldable $ coerce
  [ (0 /\ BGMColumn 0) /\ (1.25 /\ [ "01", "01" ])
  , (0 /\ BGMColumn 1) /\ (1.25 /\ [ "01", "01" ])
  , (0 /\ BGMColumn 2) /\ (1.25 /\ [ "01", "01" ])
  , (0 /\ BGMColumn 3) /\ (1.25 /\ [ "01", "01" ])
  , (1 /\ BGMColumn 0) /\ (1.00 /\ [ "01", "01" ])
  , (1 /\ BGMColumn 1) /\ (1.00 /\ [ "01", "01" ])
  , (1 /\ BGMColumn 2) /\ (1.00 /\ [ "01", "01" ])
  , (1 /\ BGMColumn 3) /\ (1.00 /\ [ "01", "01" ])
  , (2 /\ PlayColumn 0) /\ (1.00 /\ [ "01", "01" ])
  , (2 /\ PlayColumn 2) /\ (1.00 /\ [ "01", "01" ])
  , (2 /\ PlayColumn 4) /\ (1.00 /\ [ "01", "01" ])
  , (2 /\ PlayColumn 6) /\ (1.00 /\ [ "01", "01" ])
  , (3 /\ BGMColumn 0) /\ (1.00 /\ [ "01", "01" ])
  , (3 /\ BGMColumn 1) /\ (1.00 /\ [ "01", "01" ])
  , (4 /\ PlayColumn 0) /\ (1.00 /\ [ "01", "01" ])
  , (5 /\ BGMColumn 0) /\ (1.00 /\ [ "01", "01" ])
  , (5 /\ BGMColumn 1) /\ (1.00 /\ [ "01", "01" ])
  , (6 /\ BGMColumn 2) /\ (1.00 /\ [ "01", "01", "01" ])
  ]
  where
  coerce :: Array (_ /\ (Number /\ Array String))
         -> Array (_ /\ (Factor /\ Notes))
  coerce = Safe.coerce

testTiming :: Spec Unit
testTiming = describe "BMS.Timing" do
  describe "gathering measures" do
    it "should just work™" do
      (gatherAll gatherMeasuresActual).measures `shouldEqual` gatherMeasuresExpected
