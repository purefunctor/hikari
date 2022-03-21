module Hikari.Graph where

import Prelude

import Data.Tuple.Nested (type (/\))
import Hikari.Graph.BGM (TBGM)
import WAGS.Graph.AudioUnit (TGain, TSpeaker)

type FullGraph =
  ( -- The physical output device where keysounds are played.
    speaker :: TSpeaker /\ { fader :: Unit }
  -- Top-level controller node for the volume of keysounds.
  , fader :: TGain /\ { bgm :: Unit }
  -- Audio source node for background notes.
  , bgm :: TBGM /\ {}
  )
