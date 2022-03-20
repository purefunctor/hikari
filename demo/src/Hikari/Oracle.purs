module Hikari.Oracle where

import Prelude

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
import Hikari.Types (Residuals, World, playKeySound)
import Partial.Unsafe (unsafePartial)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Run (RunAudio, RunEngine, TriggeredScene(..))

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
oracle (TriggeredScene _) accumulator = pure accumulator
