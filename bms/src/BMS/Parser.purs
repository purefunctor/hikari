module BMS.Parser where

import Prelude

import BMS.Types (Instruction(..), Note(..))
import Data.Array (any, (..))
import Data.Int as Int
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty)
import Data.Number as Number
import Text.Parsing.StringParser (Parser, fail, try)
import Text.Parsing.StringParser.CodePoints (char, regex, skipSpaces, string, whiteSpace)
import Text.Parsing.StringParser.Combinators (choice, many1, sepEndBy)

-- Header Instructions

genre :: Parser Instruction
genre = Genre <$> header_ "GENRE"

title :: Parser Instruction
title = Title <$> header_ "TITLE"

artist :: Parser Instruction
artist = Artist <$> header_ "ARTIST"

bpm :: Parser Instruction
bpm = BPM <$> (char '#' *> string "BPM" *> skipSpaces *> number)

number :: Parser Number
number = fromMaybe' "not a number" $ Number.fromString <$> regex "[0-9][0-9]*(\\.[0-9][0-9]*)?"

subtitle :: Parser Instruction
subtitle = Subtitle <$> header_ "SUBTITLE"

stagefile :: Parser Instruction
stagefile = Stagefile <$> header_ "STAGEFILE"

banner :: Parser Instruction
banner = Banner <$> header_ "BANNER"

wav :: Parser Instruction
wav = string "#WAV" *> (Wav <$> ({ note: _, soundFile: _ } <$> note <*> (skipSpaces *> soundFile)))
  where
  soundFile = regex ".+"

-- Note Instructions

timeSignature :: Parser Instruction
timeSignature = measured number >>= \m ->
  if m.channel == 2 then
    pure $ TimeSignature { measure: m.measure, factor: m.rhs }
  else
    fail "not a time signature instruction"

backgroundNote :: Parser Instruction
backgroundNote = measured notes >>= \m ->
  if m.channel == 1 then
    pure $ BackgroundNote { measure: m.measure, notes: m.rhs }
  else
    fail "not a background note instruction"

gameplayNote :: Parser Instruction
gameplayNote = measured notes >>= \m ->
  if any (m.channel == _) (11 .. 16 <> [ 18, 19 ] <> 21 .. 26 <> [ 28, 29 ]) then
    pure $ GameplayNote { measure: m.measure, lane: m.channel, notes: m.rhs }
  else
    fail "not a gameplay note instruction"

-- Top-level

header :: Parser Instruction
header = choice $ try <$>
  [ genre
  , title
  , artist
  , bpm
  , subtitle
  , stagefile
  , banner
  , wav
  ]

headers :: Parser (List Instruction)
headers = header `sepEndBy` whiteSpace

measure :: Parser Instruction
measure = choice $ try <$>
  [ timeSignature
  , backgroundNote
  , gameplayNote
  ]

measures :: Parser (List Instruction)
measures = measure `sepEndBy` whiteSpace

-- Combinators

header_ :: String -> Parser String
header_ n = char '#' *> string n *> skipSpaces *> regex ".+"

measured :: forall rhs. Parser rhs -> Parser { measure :: Int, channel :: Int, rhs :: rhs }
measured rhsParser = char '#' *> ({ measure: _, channel: _, rhs: _ } <$> measureInt <*> channelInt)
  <*> (char ':' *> rhsParser)
  where
  measureInt = fromMaybe' "not a measure" (Int.fromString <$> regex "[0-9]{3}")
  channelInt = fromMaybe' "not a channel" (Int.fromString <$> regex "[0-9]{2}")

note :: Parser Note
note = Note <$> regex "[0-9a-zA-Z]{2}"

notes :: Parser (NonEmpty List Note)
notes = unwrap <$> many1 note

fromMaybe' :: forall a. String -> Parser (Maybe a) -> Parser a
fromMaybe' error parser = parser >>= case _ of
  Just result ->
    pure result
  Nothing ->
    fail error
