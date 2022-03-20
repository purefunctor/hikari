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
  { keySoundFn ::
      { bgm :: Cofree Identity KeySoundFn
      }
  }

initialAccumulator :: Accumulator
initialAccumulator =
  { keySoundFn:
      { bgm: keySoundFnCf.bgm
      }
  }

-- INTERNALS!!! BE WARNED!!!

type OfValues v =
  ( bgm :: v
  )

-- Each audio source node in the graph has a cycling stream of
-- `KeySoundFn`s.
keySoundFnCf :: { | OfValues (Cofree Identity KeySoundFn) }
keySoundFnCf =
  { bgm: mkKeySoundFnCf (Proxy :: _ "bgm") d31 false
  }

-- Builds the corresponding `KeySoundFn` for an audio source node
-- given the subgraph index.
keySoundFnHd
  :: forall nat
   . Nat nat
  => Lt nat D32
  => nat
  -> Boolean
  -> { | OfValues KeySoundFn
     }
keySoundFnHd index switch =
  { bgm: KeySoundFn
      ( \input -> ichange' (Proxy :: _ "bgm")
          (subgraphSingleSetter index $ Just { note: input.note, offset: input.offset, switch })
      )
  }

-- A helper induction for building the looping `KeySoundFn` stream.
class MkKeySoundFnCf :: Symbol -> Type -> Constraint
class MkKeySoundFnCf graphNode subgraphIndex where
  mkKeySoundFnCf :: Proxy graphNode -> subgraphIndex -> Boolean -> Cofree Identity KeySoundFn

-- At the base case, we take the already-built stream and "append" it
-- to the end, effectively creating an infinite stream.
instance
  ( IsSymbol graphNode
  , Cons graphNode (Cofree Identity KeySoundFn) _0 (OfValues (Cofree Identity KeySoundFn))
  , Cons graphNode KeySoundFn _1 (OfValues KeySoundFn)
  ) =>
  MkKeySoundFnCf graphNode D0 where
  mkKeySoundFnCf graphNode subgraphIndex switch = deferCofree
    ( \_ -> Record.get graphNode (keySoundFnHd subgraphIndex switch) /\ Identity
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
  mkKeySoundFnCf graphNode subgraphIndex switch = deferCofree
    ( \_ -> Record.get graphNode (keySoundFnHd subgraphIndex switch) /\ Identity
        (mkKeySoundFnCf graphNode (pred subgraphIndex) (not switch))
    )
