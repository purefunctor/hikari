-- | Functions from converting instructions to the core types.
module BMS.Convert where

import Prelude

import BMS.Types (Instruction(..), Measure(..), Note(..))
import Data.Foldable (class Foldable, maximum, minimum)
import Data.Foldable as Foldable
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), filter, (..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))

type FactorMap = Map Int Number

type ColumnsMap = Map Int (NonEmpty List (NonEmpty List Note))

-- | Converts a collection of instructions into a non-empty list of
-- | measures ordered by index. If the zeroth measure is not defined,
-- | this function returns `Nothing`. Likewise, this function also
-- | fills in missing measures with empty instructions.
convertNoteInstructions
  :: forall f. Foldable f => f Instruction -> Maybe (NonEmpty List Measure)
convertNoteInstructions = convertToMeasures <=< fillEmptyMeasures <<< groupInstructionsByIndex

-- | Groups instructions based on their index, yielding a `FactorMap`
-- | and a `ColumnsMap`.
groupInstructionsByIndex :: forall f. Foldable f => f Instruction -> FactorMap /\ ColumnsMap
groupInstructionsByIndex = Foldable.foldl go (Map.empty /\ Map.empty)
  where
  go (f /\ n) = case _ of
    TimeSignature { measure, factor } ->
      Map.insert measure factor f /\ n
    BackgroundNote { measure, notes } ->
      f /\ Map.insertWith (<>) measure (notes :| Nil) n
    GameplayNote { measure, notes } ->
      f /\ Map.insertWith (<>) measure (notes :| Nil) n
    _ ->
      f /\ n

-- | Fills in measures not defined in the `ColumnsMap` with empty
-- | instructions, ensuring that when computing timings down the line,
-- | the offsets are properly computed.
fillEmptyMeasures :: FactorMap /\ ColumnsMap -> Maybe (FactorMap /\ ColumnsMap)
fillEmptyMeasures (f /\ n) = do
  let nKeys = Map.keys n
  nMinimum <- minimum nKeys
  nMaximum <- maximum nKeys
  let
    missing = filter (flip Set.member nKeys) (nMinimum .. nMaximum)
    default = Map.fromFoldable $ (_ /\ ((Note "00" :| Nil) :| Nil)) <$> missing
  pure (f /\ Map.union n default)

-- | Converts the `FactorMap` and the `ColumnsMap` into an ordered,
-- | non-empty list of `Measure`s.
convertToMeasures :: FactorMap /\ ColumnsMap -> Maybe (NonEmpty List Measure)
convertToMeasures (f /\ n) = do
  columns <- Map.lookup 0 n
  let
    index = 0
    factor = fromMaybe 1.0 $ Map.lookup 0 f
  pure (Measure { index, factor, columns } :| List.reverse rest)
  where
  zeroLessF = Map.delete 0 f
  rest = foldlWithIndex go Nil (Map.delete 0 n)
    where
    go index measures columns =
      let
        factor = fromMaybe 1.0 $ Map.lookup 0 zeroLessF
      in
        Measure { index, factor, columns } : measures

-- | Gets the file names for each note.
findWavs :: forall f. Foldable f => f Instruction -> List (Note /\ String)
findWavs = Foldable.foldl go Nil
  where
  go accumulator = case _ of
    Wav { note, soundFile } ->
      (note /\ soundFile) : accumulator
    _ ->
      accumulator
