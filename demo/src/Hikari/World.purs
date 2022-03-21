module Hikari.World where

import BMS.Timing (NoteOffsets)
import BMS.Types (Note)
import Data.Map (Map)
import WAGS.WebAPI (BrowserAudioBuffer)

type KeySoundBuffers = Map Note BrowserAudioBuffer

type World =
  { keySoundBuffers :: KeySoundBuffers
  , noteOffsets :: NoteOffsets
  }
