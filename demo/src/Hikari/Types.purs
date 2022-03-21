module Hikari.Types where

import BMS.Types (Note, Offset)
import Data.Unit (Unit)
import Hikari.Graph (FullGraph)
import Hikari.Residuals (Residuals)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Run (RunAudio, RunEngine)
import WAGS.WebAPI (BrowserAudioBuffer)

newtype KeySoundFn = KeySoundFn
  ( forall proof
     . { buffer :: BrowserAudioBuffer
       , offset :: Offset
       }
    -> IxWAG RunAudio RunEngine proof Residuals FullGraph FullGraph Unit
  )

playKeySound
  :: KeySoundFn
  -> ( forall proof
        . { buffer :: BrowserAudioBuffer
          , offset :: Offset
          }
       -> IxWAG RunAudio RunEngine proof Residuals FullGraph FullGraph Unit
     )
playKeySound (KeySoundFn fn) = fn
