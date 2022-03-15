module Hikari.Oracle where

import Prelude

import BMS.Types (Note(..))
import Control.Comonad (class Comonad, extract)
import Control.Comonad.Cofree.Class (class ComonadCofree, unwrapCofree)
import Control.Monad.State (execStateT, get, lift, put)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.NonEmpty ((:|))
import Data.Traversable (class Foldable, traverse_)
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Hikari.Accumulator (Accumulator)
import Hikari.FullGraph (FullGraph)
import Hikari.Types (Residuals, World, playSound)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Run (RunAudio, RunEngine, TriggeredScene(..))
import Partial.Unsafe (unsafePartial)

cofreeTraversal_
  :: ∀ m a l f w b
   . Monad m
  => Foldable l
  => Comonad w
  => Comonad f
  => ComonadCofree f w
  => (a -> b -> m Unit)
  -> l a
  -> w b
  -> m (w b)
cofreeTraversal_ f = execStateT
  <<< traverse_
    ( \i ->
        get >>= (*>)
          <$> (lift <<< f i <<< extract)
          <*> (put <<< extract <<< unwrapCofree)
    )

oracle
  :: forall proof
   . TriggeredScene Unit World ()
  -> Accumulator
  -> IxWAG RunAudio RunEngine proof Residuals FullGraph FullGraph Accumulator
oracle (TriggeredScene { time: wagsTime }) accumulator = do
  let split = List.span (\(noteTime /\ _) -> noteTime - wagsTime < 0.50) accumulator.noteTimings
  let initFlat = List.foldl (\a (t /\ (nh :| nr)) -> a <> (t /\ nh) : ((t /\ _) <$> nr)) Nil split.init
  sounds <- cofreeTraversal_
    (\(noteTime /\ (Note note)) sound ->
       if note == "00" then
         pure unit
       else
         let buffer = unsafePartial (fromJust (Map.lookup note accumulator.noteSounds))
         in playSound sound { buffer, timeOffset: max 0.0 (noteTime - wagsTime) }
      ) initFlat accumulator.sounds
  pure $ accumulator { noteTimings = split.rest, sounds = sounds }
