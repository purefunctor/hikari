module Hikari.Engine where

import Prelude

import Control.Monad.Indexed ((:*>))
import Hikari.Accumulator (Accumulator)
import Hikari.Accumulator as Accumulator
import Hikari.Graph (FullGraph)
import Hikari.Engine.BGM as EngineBGM
import Hikari.Residuals (Residuals)
import Hikari.World (World)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Control.Functions.Graph (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Create.Optionals (gain, speaker)
import WAGS.Run (RunAudio, RunEngine, TriggeredScene)

type Environment = TriggeredScene Unit World ()

initialize :: IxWAG RunAudio RunEngine Frame0 Residuals () FullGraph Unit
initialize = icreate $ speaker { fader: gain 1.0 { bgm: EngineBGM.bgmFader } }

setup :: Environment -> IxWAG RunAudio RunEngine Frame0 Residuals FullGraph FullGraph Accumulator
setup _ = ichange' (Proxy :: _ "fader") 1.0 $> Accumulator.initial

loop
  :: forall proof
   . Environment
  -> Accumulator
  -> IxWAG RunAudio RunEngine proof Residuals FullGraph FullGraph Accumulator
loop _ = pure

scene :: Scene Environment RunAudio RunEngine Frame0 Residuals
scene = (\e -> initialize :*> setup e) @!> iloop loop
