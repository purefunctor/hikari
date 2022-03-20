module Hikari.Setup where

import Prelude

import Hikari.Accumulator (Accumulator)
import Hikari.Accumulator as Accumulator
import Hikari.Graph (FullGraph)
import Hikari.Types (Residuals, World)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Control.Indexed (IxWAG)
import WAGS.Run (RunAudio, RunEngine, TriggeredScene(..))

setup
  :: forall proof
   . TriggeredScene Unit World ()
  -> IxWAG RunAudio RunEngine proof Residuals FullGraph FullGraph Accumulator
setup (TriggeredScene _) = ichange' (Proxy :: _ "fader") 1.0 $> Accumulator.initial
