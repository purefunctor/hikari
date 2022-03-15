module Hikari.Accumulator where

import Prelude

import Control.Comonad.Cofree (Cofree, deferCofree)
import Data.Function (($))
import Data.Identity (Identity(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.NonEmpty (NonEmpty)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num
  ( class Pred
  , D0
  , D1
  , D10
  , D11
  , D12
  , D13
  , D14
  , D15
  , D16
  , D17
  , D18
  , D19
  , D2
  , D20
  , D21
  , D22
  , D23
  , D24
  , D25
  , D26
  , D27
  , D28
  , D29
  , D3
  , D30
  , D31
  , D4
  , D5
  , D6
  , D7
  , D8
  , D9
  , d31
  , pred
  )
import Hikari.FullGraph (FullGraph)
import Hikari.Types (NoteSounds, NoteTimings, PlaySound(..))
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Cons)
import Prim.Symbol (class Append)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Graph.AudioUnit (TPlayBuf)
import WAGS.Graph.Parameter (AudioOnOff(..), _offOn)
import WAGS.WebAPI (BrowserAudioBuffer)

type Accumulator =
  { sounds :: Cofree Identity PlaySound
  , noteSounds :: NoteSounds
  , noteTimings :: NoteTimings
  }

initialAccumulator :: NoteSounds -> NoteTimings -> Accumulator
initialAccumulator noteSounds noteTimings =
  { sounds
  , noteSounds
  , noteTimings
  }

class CofreeN2S (n :: Type) where
  cofreeN2S :: n -> Cofree Identity PlaySound

instance CofreeN2S D0 where
  cofreeN2S _ = deferCofree
    ( \_ -> Tuple
        ( PlaySound
            ( \{ buffer } -> ichange' (Proxy :: _ "wav0")
                { onOff: _offOn, buffer }
            )
        )
        (Identity sounds)
    )
else instance
  ( IsSymbol wavs
  , Cons wavs (Tuple TPlayBuf {}) r' FullGraph
  , N2S n s
  , Append "wav" s wavs
  , Pred n n'
  , CofreeN2S n'
  ) =>
  CofreeN2S n where
  cofreeN2S i = deferCofree
    ( \_ -> Tuple
        ( PlaySound
            ( \{ buffer, timeOffset } ->
                ichange' (Proxy :: _ wavs)
                  { onOff: AudioOnOff { onOff: _offOn, timeOffset }, buffer }
            )
        )
        (Identity (cofreeN2S (pred i)))
    )

sounds :: Cofree Identity PlaySound
sounds = cofreeN2S d31

class N2S :: Type -> Symbol -> Constraint
class N2S n s | n -> s

instance N2S D0 "0"
instance N2S D1 "1"
instance N2S D2 "2"
instance N2S D3 "3"
instance N2S D4 "4"
instance N2S D5 "5"
instance N2S D6 "6"
instance N2S D7 "7"
instance N2S D8 "8"
instance N2S D9 "9"
instance N2S D10 "10"
instance N2S D11 "11"
instance N2S D12 "12"
instance N2S D13 "13"
instance N2S D14 "14"
instance N2S D15 "15"
instance N2S D16 "16"
instance N2S D17 "17"
instance N2S D18 "18"
instance N2S D19 "19"
instance N2S D20 "20"
instance N2S D21 "21"
instance N2S D22 "22"
instance N2S D23 "23"
instance N2S D24 "24"
instance N2S D25 "25"
instance N2S D26 "26"
instance N2S D27 "27"
instance N2S D28 "28"
instance N2S D29 "29"
instance N2S D30 "30"
instance N2S D31 "31"
