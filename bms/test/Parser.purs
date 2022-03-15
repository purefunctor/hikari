module Test.Parser where

import Prelude

import BMS.Parser (bms)
import BMS.Types (BmsLine(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

known :: String
known =
  """#GENRE ONCE UPON A TIME
#TITLE GAMEBOY
#ARTIST Silentroom × 駿
#BPM 105

#SUBTITLE [LOAD]
#STAGEFILE stagefile.png
#BANNER banner.png

#WAV01 sound_file.wav

#00002:1.25
#00100:0101
"""

testParser :: Spec Unit
testParser = describe "BMS.Parser" do
  describe "bms" do
    it "should parse multiple instructions" do
      bms known `shouldEqual`
        [ Genre "ONCE UPON A TIME"
        , Title "GAMEBOY"
        , Artist "Silentroom × 駿"
        , BPM 105.0
        , Subtitle "[LOAD]"
        , Stagefile "stagefile.png"
        , Banner "banner.png"
        , Wav { file: "sound_file.wav", name: "01" }
        , ChangeFactor { factor: 1.25, measure: 0 }
        , NoteColumn { channel: 0, measure: 1, notes: [ "01", "01" ] }
        ]
    it "should parse instructions partially" do
      bms "#WAV01 sound_file.wav\n#WAV.. sound_file.wav" `shouldEqual`
        [ Wav { file: "sound_file.wav", name: "01" }
        ]
    describe "wav" do
      it "should parse base36 only" do
        bms "#WAV01 sound_file.wav" `shouldEqual`
          [ Wav { file: "sound_file.wav", name: "01" }
          ]
        bms "#WAV00 sound_file.wav" `shouldEqual` []
        bms "#WAV.. sound_file.wav" `shouldEqual` []
        bms "#WAVaa sound_file.wav" `shouldEqual` []
    describe "factor" do
      it "should be a floating factor" do
        bms "#00002:1.25" `shouldEqual`
          [ ChangeFactor { factor: 1.25, measure: 0 }
          ]
        bms "#00002:1" `shouldEqual` []
    describe "notes" do
      it "should not parse channel 02" do
        bms "#00002:01" `shouldEqual` []
        bms "#00001:01" `shouldEqual`
          [ NoteColumn { channel: 1, measure: 0, notes: [ "01" ] }
          ]
      it "should drop odd notes" do
        bms "#00001:01010" `shouldEqual`
          [ NoteColumn { channel: 1, measure: 0, notes: [ "01", "01" ] }
          ]
      it "should parse base36 only" do
        bms "#00001:aa" `shouldEqual` []
