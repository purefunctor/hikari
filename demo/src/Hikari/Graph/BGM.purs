module Hikari.Graph.BGM where

import Prelude

import BMS.Types (Note, Offset)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Num (D32)
import Data.Vec (Vec)
import WAGS.Graph.AudioUnit (Subgraph, TGain, TPlayBuf, TSubgraph)
import WAGS.Interpret (AsSubgraph)

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
