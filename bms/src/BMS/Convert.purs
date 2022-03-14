module BMS.Convert where

import Prelude

import BMS.Parser (measures)
import BMS.Timing (scoreTimings)
import BMS.Types (Instruction(..), Measure(..), Note(..))
import Data.Either (Either(..))
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
import Partial.Unsafe (unsafePartial)
import Text.Parsing.StringParser (runParser)

type FactorMap = Map Int Number

type NotesMap = Map Int (NonEmpty List (NonEmpty List Note))

-- | Try to convert a collection of instructions into a list of measures
-- | ordered by index. If the zeroth measure is not defined, returns
-- | `Nothing`.
convert :: forall f. Foldable f => f Instruction -> Maybe (NonEmpty List Measure)
convert = convertToMeasures <=< fillEmptyMeasures <<< groupInstructionsByIndex

-- | Group instructions based on their index, yielding a `Map` of indices
-- | to the measures' factors, and a `Map` of indices to the measures' notes.
groupInstructionsByIndex :: forall f. Foldable f => f Instruction -> FactorMap /\ NotesMap
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

-- | Fill measures not defined in the `NotesMap` with empty instructions.
-- | This ensures that when computing the timings for the notes, empty
-- | measures are taken account.
fillEmptyMeasures :: FactorMap /\ NotesMap -> Maybe (FactorMap /\ NotesMap)
fillEmptyMeasures (f /\ n) = do
  let nKeys = Map.keys n
  nMinimum <- minimum nKeys
  nMaximum <- maximum nKeys
  let
    missing = filter (flip Set.member nKeys) (nMinimum .. nMaximum)
    default = Map.fromFoldable $ (_ /\ ((Note "00" :| Nil) :| Nil)) <$> missing
  pure (f /\ Map.union n default)

convertToMeasures :: FactorMap /\ NotesMap -> Maybe (NonEmpty List Measure)
convertToMeasures (f /\ n) = do
  notes <- Map.lookup 0 n
  let
    index = 0
    factor = fromMaybe 1.0 $ Map.lookup 0 f
  pure (Measure { index, factor, notes } :| List.reverse rest)
  where
  zeroLessF = Map.delete 0 f
  rest = foldlWithIndex go Nil (Map.delete 0 n)
    where
    go index measures notes =
      let
        factor = fromMaybe 1.0 $ Map.lookup 0 zeroLessF
      in
        Measure { index, factor, notes } : measures

-- Tests

-- converted :: Maybe (Map Number (NonEmpty List Note))
converted = map (Map.keys <<< scoreTimings 105.0) <<< convert $ bmsNote'

bmsNote :: String
bmsNote =
  """#00002:1.00
#00001:A40000A500000000
#00011:AY0000AZ00000000

#00201:A40000A500000000
#00211:AY0000AZ00000000
"""

bmsNote' :: List Instruction
bmsNote' = unsafePartial case runParser measures bmsNote of
  Right (x : xs) -> x : xs