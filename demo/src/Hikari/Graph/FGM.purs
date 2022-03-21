module Hikari.Graph.FGM where

import Prelude

import BMS.Types (Note, Offset)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Num (D32)
import WAGS.Graph.AudioUnit (TGain, TPlayBuf, TSubgraph)
import WAGS.WebAPI (BrowserAudioBuffer)

type Name = "fgmFader"

type Count = D32

type Environment = Maybe
  { buffer :: BrowserAudioBuffer
  , offset :: Offset
  , switch :: Boolean
  }

type Graph =
  ( fgmFader :: TGain /\ { fgmA :: Unit, fgmB :: Unit }
  , fgmA :: TPlayBuf /\ {}
  , fgmB :: TPlayBuf /\ {}
  )

type TFGM = TSubgraph Count Name () Environment
