module Hikari.Setup where

import Prelude

import Hikari.Accumulator (Accumulator, initialAccumulator)
import Hikari.FullGraph (FullGraph)
import Hikari.Types (Residuals, World)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Control.Indexed (IxWAG)
import WAGS.Run (RunAudio, RunEngine, TriggeredScene(..))

setup
  :: forall proof
   . TriggeredScene Unit World ()
  -> IxWAG RunAudio RunEngine proof Residuals FullGraph FullGraph Accumulator
setup (TriggeredScene { world: { noteSounds, noteTimings } }) = do
  ichange' (Proxy :: _ "fader") 1.0
  pure (initialAccumulator noteSounds noteTimings)
