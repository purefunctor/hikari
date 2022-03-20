module Hikari.SubGraph where

import Prelude

import BMS.Types (Note, Offset)
import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Data.Vec (fill)
import Hikari.FullGraph (BGMGraph, BGMSig)
import WAGS.Control.Functions.Subgraph as SG
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Interpret (class AudioInterpret, AsSubgraph(..))
import WAGS.Patch (ipatch)

createFrameSub
  :: forall res audio engine
   . AudioInterpret audio engine
  => IxWAG audio engine Frame0 res () BGMGraph Unit
createFrameSub = ipatch
  { microphone: empty
  , mediaElement: empty
  , subgraphs: { bgm }
  , tumults: {}
  }

subFrameLoop
  :: forall res proof audio engine
   . AudioInterpret audio engine
  => Maybe { note :: Note, offset :: Offset }
  -> Unit
  -> IxWAG audio engine proof res BGMGraph BGMGraph Unit
subFrameLoop _ _ = pure unit

bgm :: BGMSig "bgm"
bgm = CTOR.Subgraph
  { subgraphMaker: AsSubgraph
      ( const $ SG.istart (\_ -> createFrameSub) (SG.iloop subFrameLoop)
      )
  , envs: fill $ const Nothing
  }
