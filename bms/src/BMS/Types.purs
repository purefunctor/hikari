module BMS.Types where

import Prelude

import Data.Array as Array
import Data.Foldable as Foldable
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)

data BmsLine
  = Genre String
  | Title String
  | Artist String
  | BPM Number
  | Subtitle String
  | Stagefile String
  | Banner String
  -- #WAVxx yy
  --
  -- registers a file to a name
  --
  -- * xx - name (string)
  -- * yy - file (string)
  | Wav { name :: String, file :: String }
  -- #xxx02:zz
  --
  -- changes the time signature
  --
  -- * xxx - measure (int)
  -- * zz  - factor  (float)
  | ChangeFactor { measure :: Int, factor :: Number }
  -- #xxxyy:zz
  --
  -- places a stream of notes
  --
  -- * xxx - measure  (int)
  -- * yy  - channel  (int)
  -- * zz  - commands (string) [0-9a-zA-z]
  | NoteColumn { measure :: Int, channel :: Int, notes :: Array String }

derive instance Eq BmsLine
derive instance Ord BmsLine
derive instance Generic BmsLine _
instance Show BmsLine where
  show = genericShow

isBGMChannel :: Int -> Boolean
isBGMChannel = eq 1

isFactorChannel :: Int -> Boolean
isFactorChannel = eq 2

isPlayChannel :: Int -> Boolean
isPlayChannel i = Foldable.or
  [ i >= 11 && i <= 16
  , i == 18 || i == 19
  , i >= 21 && i <= 26
  , i == 28 || i == 29
  ]
