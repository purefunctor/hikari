module Hikari.Graph.BGM where

import Prelude

import BMS.Types (Note, Offset)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Num (D32)
import WAGS.Graph.AudioUnit (TGain, TPlayBuf, TSubgraph)
import WAGS.WebAPI (BrowserAudioBuffer)

type Name = "bgmFader"

type Count = D32

type Environment = Maybe
  { buffer :: BrowserAudioBuffer
  , offset :: Offset
  , switch :: Boolean
  }

type Graph =
  ( bgmFader :: TGain /\ { bgmA :: Unit, bgmB :: Unit }
  , bgmA :: TPlayBuf /\ {}
  , bgmB :: TPlayBuf /\ {}
  )

type TBGM = TSubgraph Count Name () Environment
