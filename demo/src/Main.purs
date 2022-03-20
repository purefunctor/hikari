module Main where

import Prelude

import Effect (Effect)

-- import BMS.Convert (convertNoteInstructions, findWavs)
-- import BMS.Parser (headers, measures)
-- import BMS.Timing (scoreTimings')
-- import BMS.Types (Note(..))
-- import Control.Parallel (parallel, sequential)
-- import Data.Either (fromRight)
-- import Data.List (List(..))
-- import Data.Map as Map
-- import Data.Maybe (fromMaybe)
-- import Data.String (split)
-- import Data.String.Pattern (Pattern(..))
-- import Data.Traversable (for)
-- import Data.Tuple.Nested ((/\))
-- import Effect (Effect)
-- import Effect.Aff (launchAff_)
-- import Effect.Class (liftEffect)
-- import Effect.Class.Console (log, logShow)
-- import Effect.Exception.Unsafe (unsafeThrow)
-- import FRP.Event (subscribe)
-- import FRP.Event.Time as Time
-- import Hikari.Engine (makePiece)
-- import Hikari.Fetch (fetchText)
-- import Hikari.Oracle (oracle)
-- import Hikari.Setup (setup)
-- import Hikari.Types (Residuals)
-- import Text.Parsing.StringParser (runParser)
-- import WAGS.Interpret (context, decodeAudioDataFromUri, makeFFIAudioSnapshot)
-- import WAGS.Run (TriggeredRun, runNoLoop)

main :: Effect Unit
main = pure unit

-- main :: Effect Unit
-- main = launchAff_ do
--   audioCtx <- liftEffect context
--   ffiAudio <- liftEffect $ makeFFIAudioSnapshot audioCtx

--   headers <- fromRight Nil <<< runParser headers <$> fetchText "./sounds/headers.txt"
--   noteSounds <- map Map.fromFoldable $ sequential $ for (findWavs headers)
--     \(Note note /\ soundName) ->
--       case split (Pattern ".") soundName of
--         [ soundName', _ ] -> parallel do
--           log $ "Loading: " <> soundName'
--           (note /\ _) <$> decodeAudioDataFromUri audioCtx ("./sounds/" <> soundName' <> ".ogg")
--         _ ->
--           unsafeThrow $ "invalid file name: " <> soundName

--   log $ "Finished loading " <> show (Map.size noteSounds) <> " note sounds."

--   notes <- fromRight Nil <<< runParser measures <$> fetchText "./sounds/notes.txt"
--   let noteTimings = fromMaybe Nil $ convertNoteInstructions notes <#> scoreTimings' 105.0
--   logShow noteTimings

--   let timeEvent = Time.interval 10 $> unit

--   _ <- liftEffect $
--     subscribe (runNoLoop timeEvent (pure { noteSounds, noteTimings }) {} ffiAudio (makePiece setup oracle))
--       (\(_ :: TriggeredRun Residuals ()) -> pure unit)

--   pure unit
