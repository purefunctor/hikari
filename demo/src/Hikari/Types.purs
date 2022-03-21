module Hikari.Types where

import BMS.Types (Note, Offset)
import Data.Unit (Unit)
import Hikari.Graph (FullGraph)
import Hikari.Residuals (Residuals)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Run (RunAudio, RunEngine)

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
