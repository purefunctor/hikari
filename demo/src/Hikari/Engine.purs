module Hikari.Engine where

import Prelude

import BMS.Types (Column(..), Offset(..))
import Control.Comonad.Cofree (head, tail)
import Control.Monad.Indexed ((:*>))
import Control.Monad.State.Trans (execStateT, get, lift, put)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Lens (set)
import Data.Lens.Barlow (barlow)
import Data.Lens.Index (ix)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Hikari.Accumulator (Accumulator)
import Hikari.Accumulator as Accumulator
import Hikari.Engine.BGM as EngineBGM
import Hikari.Graph (FullGraph)
import Hikari.Residuals (Residuals)
import Hikari.Types (playKeySound)
import Hikari.World (World)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Control.Functions.Graph (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Create.Optionals (gain, speaker)
import WAGS.Run (RunAudio, RunEngine, TriggeredScene(..))

type Environment = TriggeredScene Unit World ()

initialize :: IxWAG RunAudio RunEngine Frame0 Residuals () FullGraph Unit
initialize = icreate $ speaker { fader: gain 1.0 { bgm: EngineBGM.bgmFader } }

setup :: Environment -> IxWAG RunAudio RunEngine Frame0 Residuals FullGraph FullGraph Accumulator
setup _ = ichange' (Proxy :: _ "fader") 1.0 $> Accumulator.initial

loop
  :: forall proof
   . Environment
  -> Accumulator
  -> IxWAG RunAudio RunEngine proof Residuals FullGraph FullGraph Accumulator
loop (TriggeredScene { time, world }) initialAccumulator = do
  flip execStateT initialAccumulator $ forWithIndex_ world.noteWithOffsets \column offsets -> do
    accumulator <- get
    case column of
      BGMColumn column'
        | Just { key: offset, value: note } <- Map.lookupLE (Offset time) offsets
        , Just keySoundFn <- Map.lookup column' accumulator.keySoundFn.bgm
        , Just buffer <- Map.lookup note world.keySoundBuffers -> do
            lift $ playKeySound (head keySoundFn) { buffer, offset: Offset $ max 0.0 (unwrap offset - time) }
            put $ set (barlow (Proxy :: Proxy "keySoundFn.bgm") <<< ix column')
              (unwrap $ tail keySoundFn)
              accumulator
      _ ->
        pure unit

scene :: Scene Environment RunAudio RunEngine Frame0 Residuals
scene = (\e -> initialize :*> setup e) @!> iloop loop
