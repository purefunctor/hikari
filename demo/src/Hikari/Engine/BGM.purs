module Hikari.Engine.BGM where

import Prelude

import Control.Monad.Indexed ((:*>))
import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Data.Vec (Vec, fill)
import Hikari.Graph.BGM (Count, Environment, Graph, Name)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Control.Functions.Subgraph as SG
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, SubScene)
import WAGS.Create.Optionals (subgraph)
import WAGS.Graph.AudioUnit (Subgraph)
import WAGS.Interpret (class AudioInterpret, AsSubgraph)
import WAGS.Patch (ipatch)

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

scene
  :: forall audio engine
   . AudioInterpret audio engine
  => SubScene Name () Environment audio engine Frame0 Unit
scene = SG.istart (\e -> initialize :*> setup e) (SG.iloop loop)

bgmFader :: Subgraph () (AsSubgraph Name () Environment) (Vec Count Environment) /\ {}
bgmFader = subgraph (fill $ const Nothing) (const scene) {}
