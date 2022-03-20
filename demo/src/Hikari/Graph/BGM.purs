module Hikari.Graph.BGM where

import Prelude

import BMS.Types (Note, Offset)
import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Num (D32)
import Data.Vec (Vec)
import Data.Vec (fill)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Control.Functions.Subgraph as SG
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0)
import WAGS.Graph.AudioUnit (Subgraph(..), TGain, TPlayBuf, TSubgraph)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Interpret (class AudioInterpret, AsSubgraph(..))
import WAGS.Interpret (AsSubgraph)
import WAGS.Patch (ipatch)

type Name = "bgmFader"

type Count = D32

type Input = Maybe
  { note :: Note
  , offset :: Offset
  , switch :: Boolean
  }

type Graph =
  ( bgmFader :: TGain /\ { bgmA :: Unit, bgmB :: Unit }
  , bgmA :: TPlayBuf /\ {}
  , bgmB :: TPlayBuf /\ {}
  )

type Signature = Subgraph () (AsSubgraph Name () Input) (Vec Count Input)

type TBGM = TSubgraph Count Name () Input

--

createFrameSub
  :: forall res audio engine
   . AudioInterpret audio engine
  => IxWAG audio engine Frame0 res () Graph Unit
createFrameSub = ipatch
  { microphone: empty
  , mediaElement: empty
  , subgraphs: { bgmFader }
  , tumults: {}
  }

subFrameLoop
  :: forall res proof audio engine
   . AudioInterpret audio engine
  => Input
  -> Unit
  -> IxWAG audio engine proof res Graph Graph Unit
subFrameLoop Nothing _ =
  pure unit
subFrameLoop (Just _) _ = do
  ichange' (Proxy :: _ "bgmFader") 1.0
  pure unit

bgmFader :: Signature
bgmFader = Subgraph
  { subgraphMaker: AsSubgraph
      ( const $ SG.istart (\_ -> createFrameSub) (SG.iloop subFrameLoop)
      )
  , envs: fill $ const Nothing
  }
