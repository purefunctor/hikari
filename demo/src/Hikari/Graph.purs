module Hikari.Graph where

import Prelude

import BMS.Types (Note, Offset)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Num (D32, D80)
import Data.Vec (Vec)
import Hikari.Graph.BGM (TBGM)
import WAGS.Graph.AudioUnit (Subgraph, TGain, TPlayBuf, TSpeaker, TSubgraph)
import WAGS.Interpret (AsSubgraph)

type FullGraph =
  ( -- The physical output device where keysounds are played.
    speaker :: TSpeaker /\ { fader :: Unit }
  -- Top-level controller node for the volume of keysounds.
  , fader :: TGain /\ { bgm :: Unit }
  -- Audio source node for background notes.
  , bgm :: TBGM /\ {}
  )
