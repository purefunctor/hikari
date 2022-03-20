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
  , fader :: TGain /\ { bgm0 :: Unit, bgm1 :: Unit }

  -- Audio source nodes for background keysounds. Two of these is
  -- needed such that we don't end up playing keysounds in in-use
  -- `TPlayBuf` nodes.
  , bgm0 :: TBGM /\ {}
  , bgm1 :: TBGM /\ {}
  )

type TBGM = TSubgraph D32 "bgm" () (Maybe { note :: Note, offset :: Offset })

type BGMGraph =
  ( bgm :: TPlayBuf /\ {}
  )

type BGMSig id = Subgraph
  ()
  ( AsSubgraph
      id
      ()
      (Maybe { note :: Note, offset :: Offset })
  )
  (Vec D32 (Maybe { note :: Note, offset :: Offset }))

type BGM0Sig = BGMSig "bgm0"

type BGM1Sig = BGMSig "bgm1"
