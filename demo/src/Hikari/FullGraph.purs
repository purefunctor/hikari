module Hikari.FullGraph where

import Prelude

import Data.Tuple.Nested (type (/\))
import WAGS.Graph.AudioUnit (TGain, TPlayBuf, TSpeaker)

type FullGraph =
  ( speaker :: TSpeaker /\ { fader :: Unit }
  , fader ::
      TGain /\
        { wav0 :: Unit
        , wav1 :: Unit
        , wav2 :: Unit
        , wav3 :: Unit
        , wav4 :: Unit
        , wav5 :: Unit
        , wav6 :: Unit
        , wav7 :: Unit
        , wav8 :: Unit
        , wav9 :: Unit
        , wav10 :: Unit
        , wav11 :: Unit
        , wav12 :: Unit
        , wav13 :: Unit
        , wav14 :: Unit
        , wav15 :: Unit
        , wav16 :: Unit
        , wav17 :: Unit
        , wav18 :: Unit
        , wav19 :: Unit
        , wav20 :: Unit
        , wav21 :: Unit
        , wav22 :: Unit
        , wav23 :: Unit
        , wav24 :: Unit
        , wav25 :: Unit
        , wav26 :: Unit
        , wav27 :: Unit
        , wav28 :: Unit
        , wav29 :: Unit
        , wav30 :: Unit
        , wav31 :: Unit
        }
  , wav0 :: TPlayBuf /\ {}
  , wav1 :: TPlayBuf /\ {}
  , wav2 :: TPlayBuf /\ {}
  , wav3 :: TPlayBuf /\ {}
  , wav4 :: TPlayBuf /\ {}
  , wav5 :: TPlayBuf /\ {}
  , wav6 :: TPlayBuf /\ {}
  , wav7 :: TPlayBuf /\ {}
  , wav8 :: TPlayBuf /\ {}
  , wav9 :: TPlayBuf /\ {}
  , wav10 :: TPlayBuf /\ {}
  , wav11 :: TPlayBuf /\ {}
  , wav12 :: TPlayBuf /\ {}
  , wav13 :: TPlayBuf /\ {}
  , wav14 :: TPlayBuf /\ {}
  , wav15 :: TPlayBuf /\ {}
  , wav16 :: TPlayBuf /\ {}
  , wav17 :: TPlayBuf /\ {}
  , wav18 :: TPlayBuf /\ {}
  , wav19 :: TPlayBuf /\ {}
  , wav20 :: TPlayBuf /\ {}
  , wav21 :: TPlayBuf /\ {}
  , wav22 :: TPlayBuf /\ {}
  , wav23 :: TPlayBuf /\ {}
  , wav24 :: TPlayBuf /\ {}
  , wav25 :: TPlayBuf /\ {}
  , wav26 :: TPlayBuf /\ {}
  , wav27 :: TPlayBuf /\ {}
  , wav28 :: TPlayBuf /\ {}
  , wav29 :: TPlayBuf /\ {}
  , wav30 :: TPlayBuf /\ {}
  , wav31 :: TPlayBuf /\ {}
  )
