module Hikari.FullGraph where

import Prelude

import BMS.Types (Note, Offset)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Num (D32, D80)
import Data.Vec (Vec)
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

type TBGM = TSubgraph D32 "bgmFader" ()
  (Maybe { note :: Note, offset :: Offset, switch :: Boolean })

type BGMGraph =
  ( bgmFader :: TGain /\ { bgmA :: Unit, bgmB :: Unit }
  , bgmA :: TPlayBuf /\ {}
  , bgmB :: TPlayBuf /\ {}
  )

type BGMSig = Subgraph
  ()
  ( AsSubgraph
      "bgmFader"
      ()
      (Maybe { note :: Note, offset :: Offset, switch :: Boolean })
  )
  (Vec D32 (Maybe { note :: Note, offset :: Offset, switch :: Boolean }))
