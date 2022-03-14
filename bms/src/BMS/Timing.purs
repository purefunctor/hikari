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

-- | Compute the starting positions of a measure's notes (in seconds)
-- | given the `bpm` and the measures `start` position (in seconds).
-- | Notes with the same timing are joined together to form a chord.
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

-- | Compute the starting positions (in seconds) of measures given the
-- | `bpm`.
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

-- | Compute all starting positions (in seconds) of all notes across
-- | all measures given the `bpm`. Notes with the same timing are
-- | joined together to form a chord.
scoreTimings :: Number -> NonEmpty List Measure -> Map Number (NonEmpty List Note)
scoreTimings bpm = foldlWithIndex go Map.empty <<< measureTimings bpm
  where
  go measure accumulator start =
    Map.union (noteTimings bpm start measure) accumulator

scoreTimings'
  :: forall f. Unfoldable f => Number -> NonEmpty List Measure -> f (Number /\ NonEmpty List Note)
scoreTimings' bpm = Map.toUnfoldable <<< scoreTimings bpm
