module Hikari.Engine where

import Prelude

import Control.Monad.Indexed ((:*>))
import Control.Plus (empty)
import Hikari.FullGraph (FullGraph)
import WAGS.Control.Functions.Graph (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Patch (ipatch)
import WAGS.Run (RunAudio, RunEngine)

createFrame :: forall res. IxWAG RunAudio RunEngine Frame0 res () FullGraph Unit
createFrame = ipatch { microphone: empty, mediaElement: empty }

makePiece
  :: forall acc env res
   . Monoid res
  => (forall proof. env -> IxWAG RunAudio RunEngine proof res FullGraph FullGraph acc)
  -> ( forall proof
        . env
       -> acc
       -> IxWAG RunAudio RunEngine proof res FullGraph FullGraph acc
     )
  -> Scene env RunAudio RunEngine Frame0 res
makePiece setup oracle = (\env -> (createFrame :*> setup env)) @!> iloop oracle
