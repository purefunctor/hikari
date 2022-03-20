module Hikari.Types where

import BMS.Types (Note, Offset)
import Control.Comonad.Cofree (Cofree)
import Data.Identity (Identity)
import Data.List (List)
import Data.Map (Map)
import Data.Monoid.Additive (Additive)
import Data.Monoid.Disj (Disj)
import Data.NonEmpty (NonEmpty)
import Data.Tuple.Nested (type (/\))
import Data.Unit (Unit)
import Hikari.Graph (FullGraph)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Run (RunAudio, RunEngine)
import WAGS.WebAPI (BrowserAudioBuffer)

newtype KeySoundFn = KeySoundFn
  ( forall proof
     . { note :: Note
       , offset :: Offset
       }
    -> IxWAG RunAudio RunEngine proof Residuals FullGraph FullGraph Unit
  )

playKeySound
  :: KeySoundFn
  -> ( forall proof
        . { note :: Note
          , offset :: Offset
          }
       -> IxWAG RunAudio RunEngine proof Residuals FullGraph FullGraph Unit
     )
playKeySound (KeySoundFn fn) = fn

type World =
  {
  }

type Residuals =
  {
  }
