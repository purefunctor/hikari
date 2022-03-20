module Hikari.Accumulator (Accumulator, initialAccumulator) where

import Data.Typelevel.Num
import Prelude

import Control.Comonad.Cofree (Cofree, deferCofree)
import Data.Function (($))
import Data.Identity (Identity(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.NonEmpty (NonEmpty)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Hikari.FullGraph (FullGraph)
import Hikari.Types (KeySoundFn(..))
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Cons)
import Prim.Symbol (class Append)
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Control.Indexed (IxWAG)
import WAGS.Create.Optionals (subgraphSingleSetter)
import WAGS.Graph.AudioUnit (TPlayBuf)
import WAGS.Graph.Parameter (AudioOnOff(..), _offOn)
import WAGS.WebAPI (BrowserAudioBuffer)

type Accumulator =
  { bgmKeySoundFn :: { _0 :: Cofree Identity KeySoundFn, _1 :: Cofree Identity KeySoundFn }
  }

initialAccumulator :: Accumulator
initialAccumulator =
  { bgmKeySoundFn:
      { _0: keySoundFnCf.bgm0
      , _1: keySoundFnCf.bgm1
      }
  }

-- INTERNALS!!! BE WARNED!!!

type OfValues v =
  ( bgm0 :: v
  , bgm1 :: v
  )

-- Each audio source node in the graph has a cycling stream of
-- `KeySoundFn`s.
keySoundFnCf :: { | OfValues (Cofree Identity KeySoundFn) }
keySoundFnCf =
  { bgm0: mkKeySoundFnCf (Proxy :: _ "bgm0") d31
  , bgm1: mkKeySoundFnCf (Proxy :: _ "bgm1") d31
  }

-- Builds the corresponding `KeySoundFn` for an audio source node
-- given the subgraph index.
keySoundFnHd
  :: forall nat
   . Nat nat
  => Lt nat D32
  => nat
  -> { | OfValues KeySoundFn
     }
keySoundFnHd index =
  { bgm0: KeySoundFn
      (\input -> ichange' (Proxy :: _ "bgm0") (subgraphSingleSetter index $ Just input))
  , bgm1: KeySoundFn
      (\input -> ichange' (Proxy :: _ "bgm1") (subgraphSingleSetter index $ Just input))
  }

-- A helper induction for building the looping `KeySoundFn` stream.
class MkKeySoundFnCf :: Symbol -> Type -> Constraint
class MkKeySoundFnCf graphNode subgraphIndex where
  mkKeySoundFnCf :: Proxy graphNode -> subgraphIndex -> Cofree Identity KeySoundFn

-- At the base case, we take the already-built stream and "append" it
-- to the end, effectively creating an infinite stream.
instance
  ( IsSymbol graphNode
  , Cons graphNode (Cofree Identity KeySoundFn) _0 (OfValues (Cofree Identity KeySoundFn))
  , Cons graphNode KeySoundFn _1 (OfValues KeySoundFn)
  ) =>
  MkKeySoundFnCf graphNode D0 where
  mkKeySoundFnCf graphNode subgraphIndex = deferCofree
    ( \_ -> Record.get graphNode (keySoundFnHd subgraphIndex) /\ Identity
        (Record.get graphNode keySoundFnCf)
    )

-- At the recursive case, we append `subgraphIndex - 1` to the end of
-- the stream.
else instance
  ( IsSymbol graphNode
  , Cons graphNode (Cofree Identity KeySoundFn) _0 (OfValues (Cofree Identity KeySoundFn))
  , Cons graphNode KeySoundFn _1 (OfValues KeySoundFn)
  , Lt subgraphIndex D32
  , Pred subgraphIndex predSubgraphIndex
  , MkKeySoundFnCf graphNode predSubgraphIndex
  ) =>
  MkKeySoundFnCf graphNode subgraphIndex where
  mkKeySoundFnCf graphNode subgraphIndex = deferCofree
    ( \_ -> Record.get graphNode (keySoundFnHd subgraphIndex) /\ Identity
        (mkKeySoundFnCf graphNode (pred subgraphIndex))
    )
