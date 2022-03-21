module Hikari.Engine where

import Prelude

import BMS.Types (Column(..), Offset(..))
import Control.Comonad.Cofree (head, tail)
import Control.Monad.Indexed ((:*>))
import Control.Monad.State.Trans (execStateT, get, lift, modify_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Lens (set)
import Data.Lens.Barlow (barlow)
import Data.Lens.Index (ix)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Hikari.Accumulator (Accumulator)
import Hikari.Accumulator as Accumulator
import Hikari.Engine.BGM as EngineBGM
import Hikari.Engine.FGM as EngineFGM
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
initialize = icreate $ speaker { fader: gain 1.0 { bgm: EngineBGM.bgmFader, fgm: EngineFGM.fgmFader } }

setup :: Environment -> IxWAG RunAudio RunEngine Frame0 Residuals FullGraph FullGraph Accumulator
setup (TriggeredScene { world: { notesPerColumn } }) =
  ichange' (Proxy :: _ "fader") 1.0 $> Accumulator.initial notesPerColumn

loop
  :: forall proof
   . Environment
  -> Accumulator
  -> IxWAG RunAudio RunEngine proof Residuals FullGraph FullGraph Accumulator
loop (TriggeredScene { time, world }) initialAccumulator = do
  flip execStateT initialAccumulator do
    accumulator <- get
    forWithIndex_ accumulator.notesPerColumn \column offsets -> do
      case column of
        BGMColumn column'
          | Just { head: offset /\ note, tail: offsets' } <- List.uncons offsets
          , unwrap offset - time < 0.10
          , Just keySoundFn <- Map.lookup column' accumulator.keySoundFn.bgm
          , Just buffer <- Map.lookup note world.keySoundBuffers -> do
              lift $ playKeySound (head keySoundFn) { buffer, offset: Offset $ max 0.0 (unwrap offset - time) }
              modify_ $ set (barlow (Proxy :: Proxy "keySoundFn.bgm") <<< ix column') (unwrap $ tail keySoundFn)
              modify_ $ set (barlow (Proxy :: Proxy "notesPerColumn") <<< ix column) offsets'
        PlayColumn column'
          | Just { head: offset /\ note, tail: offsets' } <- List.uncons offsets
          , unwrap offset - time < 0.10
          , Just keySoundFn <- Map.lookup column' accumulator.keySoundFn.fgm
          , Just buffer <- Map.lookup note world.keySoundBuffers -> do
              lift $ playKeySound (head keySoundFn) { buffer, offset: Offset $ max 0.0 (unwrap offset - time) }
              modify_ $ set (barlow (Proxy :: Proxy "keySoundFn.fgm") <<< ix column') (unwrap $ tail keySoundFn)
              modify_ $ set (barlow (Proxy :: Proxy "notesPerColumn") <<< ix column) offsets'
        _ ->
          pure unit

scene :: Scene Environment RunAudio RunEngine Frame0 Residuals
scene = (\e -> initialize :*> setup e) @!> iloop loop
