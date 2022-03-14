-- | Functions for computing the timings of measures and notes.
module BMS.Timing where

import Prelude

import BMS.Types (Measure(..), Note)
import Data.Foldable as Foldable
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), zip, (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.NonEmpty (NonEmpty, (:|))
import Data.Semigroup.Foldable (foldl1)
import Data.Tuple.Nested (type (/\))
import Data.Unfoldable (class Unfoldable)

-- | Computes the starting positions of a measure's notes given the
-- | `bpm` and the `start` position of the measure. Notes starting
-- | at the same time are joined together to form a chord.
noteTimings :: Number -> Number -> Measure -> Map Number (NonEmpty List Note)
noteTimings bpm start (Measure { factor, notes }) =
  let
    measureLength = 60.0 / bpm * 4.0 * factor

    laneTimings lane =
      let
        measureSubdivisions = Foldable.length lane

        subdivisionLength = measureLength / measureSubdivisions

        addTimings = go Map.empty 0.0
          where
          go timedNotes currentFactor =
            let
              timing = start + currentFactor * subdivisionLength
            in
              case _ of
                sound :| Nil ->
                  Map.insert timing (sound :| Nil) timedNotes
                sound :| next : rest ->
                  go (Map.insert timing (sound :| Nil) timedNotes) (currentFactor + 1.0)
                    (next :| rest)
      in
        addTimings lane

    mergeChords = foldl1 (Map.intersectionWith (<>))
  in
    mergeChords $ laneTimings <$> notes

noteTimings'
  :: forall f
   . Unfoldable f
  => Number
  -> Number
  -> Measure
  -> f (Number /\ NonEmpty List Note)
noteTimings' bpm start = Map.toUnfoldable <<< noteTimings bpm start

-- | Computes the starting positions of a non-empty list of measures
-- | given the `bpm`. Note that this function assumes that the given
-- | list of measures is ordered and is not missing an index.
measureTimings :: Number -> NonEmpty List Measure -> Map Measure Number
measureTimings bpm (m :| ms) = Map.fromFoldable (zip measures timings)
  where
  measures = m : ms

  timings = go 0.0 (0.0 : Nil) measures

  go currentTime measureTimes =
    case _ of
      Nil ->
        List.reverse measureTimes
      Measure { factor } : rest ->
        let
          measureLength = 60.0 / bpm * 4.0 * factor
          nextTime = currentTime + measureLength
        in
          go nextTime (nextTime : measureTimes) rest

measureTimings'
  :: forall f
   . Unfoldable f
  => Number
  -> NonEmpty List Measure
  -> f (Measure /\ Number)
measureTimings' bpm = Map.toUnfoldable <<< measureTimings bpm


-- | Computes the starting positions of all notes across a non-empty
-- | list of measures given the `bpm`. Notes starting at the same time
-- | are joined together to form a chord. Similarly to
-- | `measureTimings`, this function also assumes that the list of
-- | measures is ordered and has no missing indices.
scoreTimings :: Number -> NonEmpty List Measure -> Map Number (NonEmpty List Note)
scoreTimings bpm = foldlWithIndex go Map.empty <<< measureTimings bpm
  where
  go measure accumulator start =
    Map.union (noteTimings bpm start measure) accumulator

scoreTimings'
  :: forall f. Unfoldable f => Number -> NonEmpty List Measure -> f (Number /\ NonEmpty List Note)
scoreTimings' bpm = Map.toUnfoldable <<< scoreTimings bpm
