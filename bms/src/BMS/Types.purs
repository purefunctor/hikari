module BMS.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty)
import Data.Show.Generic (genericShow)

newtype Note = Note String

derive instance Eq Note
derive instance Ord Note
derive instance Newtype Note _

instance Show Note where
  show (Note note) = "(Note " <> note <> ")"

type Indices = NonEmpty List Note

data Instruction
  -- Header Field
  = Genre String
  | Title String
  | Artist String
  | BPM Number
  | Subtitle String
  | Stagefile String
  | Banner String
  | Wav { note :: Note, soundFile :: String }
  -- Main Data Field
  | TimeSignature { measure :: Int, factor :: Number }
  | BackgroundNote { measure :: Int, notes :: NonEmpty List Note }
  | GameplayNote { measure :: Int, column :: Int, notes :: NonEmpty List Note }

derive instance Eq Instruction
derive instance Ord Instruction
derive instance Generic Instruction _

instance Show Instruction where
  show = genericShow

newtype Measure = Measure
  { index :: Int
  , factor :: Number
  , columns :: NonEmpty List (NonEmpty List Note)
  }

derive instance Eq Measure

instance Ord Measure where
  compare (Measure m1) (Measure m2) = compare m1.index m2.index

derive instance Generic Measure _

instance Show Measure where
  show = genericShow
