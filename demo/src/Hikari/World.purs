module Hikari.World where

import Prelude

import BMS.Timing (Headers, NoteOffsets)
import BMS.Types (Note)
import Control.Parallel.Class (parallel, sequential)
import Data.Map (Map)
import Data.Map as Map
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Exception.Unsafe (unsafeThrow)
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

type KeySoundBuffers = Map Note BrowserAudioBuffer

type World =
  { keySoundBuffers :: KeySoundBuffers
  , noteWithOffsets :: NoteOffsets
  }

loadKeySoundBuffers
  :: forall headers
   . AudioContext
  -> { headers :: Headers | headers }
  -> Aff KeySoundBuffers
loadKeySoundBuffers audioCtx { headers: { wavs } } = map Map.fromFoldable $ sequential $
  traverseWithIndex go wavs
  where
  go note fileName = case split (Pattern ".") fileName of
    [ fileName', _ ] -> parallel do
      (note /\ _) <$> decodeAudioDataFromUri audioCtx ("./sounds/" <> fileName' <> ".ogg")
    _ ->
      unsafeThrow $ "Could not load file: " <> fileName
