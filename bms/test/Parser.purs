module Test.Parser where

import Prelude

import BMS.Parser (headers, measures)
import BMS.Types (Instruction(..), Note(..))
import Data.Either (Either(..))
import Data.List as List
import Data.NonEmpty ((:|))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.StringParser (runParser)

bmsMeta :: String
bmsMeta =
  """#GENRE ONCE UPON A TIME
#TITLE GAMEBOY
#ARTIST Silentroom × 駿
#BPM 105
#SUBTITLE [LOAD]
#STAGEFILE stagefile.png
#BANNER banner.png
#WAV01 violin_001.wav
#WAV02 violin_002.wav
"""

bmsNote :: String
bmsNote =
  """#00102:1.00
#00101:A40000A500000000
#00111:AY0000AZ00000000
"""

testParser :: Spec Unit
testParser = describe "BMS.Parser" do
  describe "instructions" do
    it "should parse header instructions" do
      runParser headers bmsMeta `shouldEqual`
        ( Right $ List.fromFoldable
            [ Genre "ONCE UPON A TIME"
            , Title "GAMEBOY"
            , Artist "Silentroom × 駿"
            , BPM 105.0
            , Subtitle "[LOAD]"
            , Stagefile "stagefile.png"
            , Banner "banner.png"
            , Wav { note: Note "01", soundFile: "violin_001.wav" }
            , Wav { note: Note "02", soundFile: "violin_002.wav" }
            ]
        )
    it "should parse note instructions" do
      runParser measures bmsNote `shouldEqual`
        ( Right $ List.fromFoldable
            [ TimeSignature { factor: 1.0, measure: 1 }
            , BackgroundNote
                { measure: 1
                , notes: Note "A4" :| List.fromFoldable
                    [ Note "00", Note "00", Note "A5", Note "00", Note "00", Note "00", Note "00" ]
                }
            , GameplayNote
                { measure: 1
                , lane: 11
                , notes: Note "AY" :| List.fromFoldable
                    [ Note "00", Note "00", Note "AZ", Note "00", Note "00", Note "00", Note "00" ]
                }
            ]
        )
