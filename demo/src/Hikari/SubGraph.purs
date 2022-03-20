module Hikari.SubGraph where

import Prelude

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Data.Vec (fill)
import Hikari.Graph.BGM as GraphBGM
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Control.Functions.Subgraph as SG
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Interpret (class AudioInterpret, AsSubgraph(..))
import WAGS.Patch (ipatch)

createFrameSub
  :: forall res audio engine
   . AudioInterpret audio engine
  => IxWAG audio engine Frame0 res () GraphBGM.Graph Unit
createFrameSub = ipatch
  { microphone: empty
  , mediaElement: empty
  , subgraphs: { bgmFader }
  , tumults: {}
  }

subFrameLoop
  :: forall res proof audio engine
   . AudioInterpret audio engine
  => GraphBGM.Input
  -> Unit
  -> IxWAG audio engine proof res GraphBGM.Graph GraphBGM.Graph Unit
subFrameLoop Nothing _ =
  pure unit
subFrameLoop (Just _) _ = do
  ichange' (Proxy :: _ "bgmFader") 1.0
  pure unit

bgmFader :: GraphBGM.Signature
bgmFader = CTOR.Subgraph
  { subgraphMaker: AsSubgraph
      ( const $ SG.istart (\_ -> createFrameSub) (SG.iloop subFrameLoop)
      )
  , envs: fill $ const Nothing
  }
