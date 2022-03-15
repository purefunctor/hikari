module Hikari.Types where

import BMS.Types (Note)
import Control.Comonad.Cofree (Cofree)
import Data.Identity (Identity)
import Data.List (List)
import Data.Map (Map)
import Data.Monoid.Additive (Additive)
import Data.Monoid.Disj (Disj)
import Data.NonEmpty (NonEmpty)
import Data.Tuple.Nested (type (/\))
import Data.Unit (Unit)
import Hikari.FullGraph (FullGraph)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Run (RunAudio, RunEngine)
import WAGS.WebAPI (BrowserAudioBuffer)

type NoteTimings = List (Number /\ NonEmpty List Note)

type NoteSounds = Map String BrowserAudioBuffer

newtype PlaySound = PlaySound
  ( forall proof
     . { buffer :: BrowserAudioBuffer, timeOffset :: Number }
    -> IxWAG RunAudio RunEngine proof Residuals FullGraph FullGraph Unit
  )

playSound
  :: PlaySound
  -> ( forall proof
        . { buffer :: BrowserAudioBuffer, timeOffset :: Number }
       -> IxWAG RunAudio RunEngine proof Residuals FullGraph FullGraph Unit
     )
playSound (PlaySound fn) = fn

type World =
  { noteSounds :: NoteSounds
  , noteTimings :: NoteTimings
  }

type Residuals =
  { clockTime :: Additive Number
  , finished :: Disj Boolean
  }
