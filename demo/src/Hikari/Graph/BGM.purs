module Hikari.Graph.BGM where

import Prelude

import BMS.Types (Note, Offset)
import Control.Monad.Indexed ((:*>))
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
import WAGS.Run (RunAudio, RunEngine, TriggeredScene)

type Name = "bgmFader"

type Count = D32

type Environment = Maybe
  { note :: Note
  , offset :: Offset
  , switch :: Boolean
  }

type Graph =
  ( bgmFader :: TGain /\ { bgmA :: Unit, bgmB :: Unit }
  , bgmA :: TPlayBuf /\ {}
  , bgmB :: TPlayBuf /\ {}
  )

type TBGM = TSubgraph Count Name () Environment

--

initialize
  :: forall residuals audio engine
   . AudioInterpret audio engine
  => IxWAG audio engine Frame0 residuals () Graph Unit
initialize = ipatch
  { microphone: empty
  , mediaElement: empty
  , subgraphs: { bgmFader }
  , tumults: {}
  }

setup
  :: forall residuals proof audio engine
   . AudioInterpret audio engine
  => Environment
  -> IxWAG audio engine proof residuals Graph Graph Unit
setup _ = ichange' (Proxy :: _ "bgmFader") 1.0 $> unit

loop
  :: forall residuals proof audio engine
   . AudioInterpret audio engine
  => Environment
  -> Unit
  -> IxWAG audio engine proof residuals Graph Graph Unit
loop _ _ = pure unit

bgmFader :: Subgraph () (AsSubgraph Name () Environment) (Vec Count Environment)
bgmFader = Subgraph
  { subgraphMaker: AsSubgraph
      ( const $ SG.istart (\e -> initialize :*> setup e) (SG.iloop loop)
      )
  , envs: fill $ const Nothing
  }
