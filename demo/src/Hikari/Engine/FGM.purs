module Hikari.Engine.FGM where

import Prelude

import Control.Monad.Indexed ((:*>))
import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\))
import Data.Vec (Vec, fill)
import Hikari.Graph.FGM (Count, Environment, Graph, Name)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Control.Functions.Subgraph as SG
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, SubScene)
import WAGS.Create.Optionals (subgraph)
import WAGS.Graph.AudioUnit (Subgraph)
import WAGS.Graph.Parameter (AudioOnOff(..), _offOn)
import WAGS.Interpret (class AudioInterpret, AsSubgraph)
import WAGS.Patch (ipatch)

initialize
  :: forall residuals audio engine
   . AudioInterpret audio engine
  => IxWAG audio engine Frame0 residuals () Graph Unit
initialize = ipatch
  { microphone: empty
  , mediaElement: empty
  , subgraphs: { fgmFader }
  , tumults: {}
  }

setup
  :: forall residuals proof audio engine
   . AudioInterpret audio engine
  => Environment
  -> IxWAG audio engine proof residuals Graph Graph Unit
setup _ = ichange' (Proxy :: _ "fgmFader") 1.0 $> unit

loop
  :: forall residuals proof audio engine
   . AudioInterpret audio engine
  => Environment
  -> Unit
  -> IxWAG audio engine proof residuals Graph Graph Unit
loop Nothing _ = pure unit
loop (Just { buffer, offset, switch }) _ =
  let
    argument = { buffer, onOff: AudioOnOff { onOff: _offOn, timeOffset: unwrap offset } }
  in
    if switch then
      ichange' (Proxy :: _ "fgmA") argument
    else
      ichange' (Proxy :: _ "fgmB") argument

scene
  :: forall audio engine
   . AudioInterpret audio engine
  => SubScene Name () Environment audio engine Frame0 Unit
scene = SG.istart (\e -> initialize :*> setup e) (SG.iloop loop)

fgmFader :: Subgraph () (AsSubgraph Name () Environment) (Vec Count Environment) /\ {}
fgmFader = subgraph (fill $ const Nothing) (const scene) {}
