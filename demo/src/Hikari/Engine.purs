module Hikari.Engine where

import Prelude

import Control.Monad.Indexed ((:*>))
import Control.Plus (empty)
import Data.Tuple.Nested (type (/\), (/\))
import Hikari.Graph (FullGraph)
import Hikari.Graph.BGM as GraphBGM
import WAGS.Control.Functions.Graph (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Create.Optionals (gain, speaker)
import WAGS.Patch (ipatch)
import WAGS.Run (RunAudio, RunEngine)

createFrame
  :: forall res
   . GraphBGM.Signature /\ {}
  -> IxWAG RunAudio RunEngine Frame0 res () FullGraph Unit
createFrame bgm = icreate $ speaker { fader: gain 1.0 { bgm } }

makePiece
  :: forall acc env res
   . Monoid res
  => GraphBGM.Signature
  -> (forall proof. env -> IxWAG RunAudio RunEngine proof res FullGraph FullGraph acc)
  -> ( forall proof
        . env
       -> acc
       -> IxWAG RunAudio RunEngine proof res FullGraph FullGraph acc
     )
  -> Scene env RunAudio RunEngine Frame0 res
makePiece bgm setup oracle = (\env -> (createFrame (bgm /\ {}) :*> setup env))
  @!> iloop oracle
