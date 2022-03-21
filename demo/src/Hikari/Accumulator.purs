module Hikari.Accumulator (Accumulator, initial) where

import Prelude

import BMS.Types (Column, Note, Offset)
import Control.Comonad.Cofree (Cofree, deferCofree)
import Data.Identity (Identity(..))
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (class Lt, class Nat, class Pred, D0, D32, d7, d31, pred, toInt)
import Hikari.Types (KeySoundFn(..))
import Prim.Row (class Cons)
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Create.Optionals (subgraphSingleSetter)

type Accumulator =
  { notesPerColumn :: Map Column (List (Offset /\ Note))
  , keySoundFn ::
      { bgm :: Map Int (Cofree Identity KeySoundFn)
      , fgm :: Map Int (Cofree Identity KeySoundFn)
      }
  }

initial :: Map Column (List (Offset /\ Note)) -> Accumulator
initial notesPerColumn =
  { notesPerColumn
  , keySoundFn:
    { bgm: keySoundFnCf.bgm
    , fgm: keySoundFnCf.fgm
    }
  }

-- INTERNALS!!! BE WARNED!!!

type OfValues :: forall k. k -> Row k
type OfValues v =
  ( bgm :: v
  , fgm :: v
  )

-- Each audio source node in the graph has a cycling stream of
-- `KeySoundFn`s.
keySoundFnCf :: { | OfValues (Map Int (Cofree Identity KeySoundFn)) }
keySoundFnCf =
  { bgm: mkKeySoundFnCf (Proxy :: _ "bgm") d31
  , fgm: mkKeySoundFnCf (Proxy :: _ "fgm") d7
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
          (subgraphSingleSetter index $ Just { buffer: input.buffer, offset: input.offset, switch })
      )
  , fgm: KeySoundFn
     ( \input -> ichange' (Proxy :: _ "fgm")
          (subgraphSingleSetter index $ Just { buffer: input.buffer, offset: input.offset, switch })
     )
  }

-- A helper induction for building the looping `KeySoundFn` stream.
class MkKeySoundFnCf :: Symbol -> Type -> Constraint
class MkKeySoundFnCf graphNode subgraphIndex where
  mkKeySoundFnCf :: Proxy graphNode -> subgraphIndex -> Map Int (Cofree Identity KeySoundFn)

-- At the base case, we take the already-built stream and "append" it
-- to the end, effectively creating an infinite stream.
instance
  ( IsSymbol graphNode
  , Cons graphNode KeySoundFn _0 (OfValues KeySoundFn)
  ) =>
  MkKeySoundFnCf graphNode D0 where
  mkKeySoundFnCf graphNode subgraphIndex =
    let
      keySoundFnHd' = keySoundFnHd subgraphIndex
      keySoundFnCf' =
        deferCofree
          ( \_ -> Record.get graphNode (keySoundFnHd' false) /\ Identity
              ( deferCofree
                  ( \_ -> Record.get graphNode (keySoundFnHd' true) /\ Identity keySoundFnCf'
                  )
              )
          )
    in
      Map.singleton 0 keySoundFnCf'

-- At the recursive case, we append `subgraphIndex - 1` to the end of
-- the stream.
else instance
  ( IsSymbol graphNode
  , Cons graphNode KeySoundFn _0 (OfValues KeySoundFn)
  , Lt subgraphIndex D32
  , Pred subgraphIndex predSubgraphIndex
  , MkKeySoundFnCf graphNode predSubgraphIndex
  ) =>
  MkKeySoundFnCf graphNode subgraphIndex where
  mkKeySoundFnCf graphNode subgraphIndex =
    let
      keySoundFnHd' = keySoundFnHd subgraphIndex
      keySoundFnCf' =
        deferCofree
          ( \_ -> Record.get graphNode (keySoundFnHd' false) /\ Identity
              ( deferCofree
                  ( \_ -> Record.get graphNode (keySoundFnHd' true) /\ Identity keySoundFnCf'
                  )
              )
          )
    in
      Map.insert
        (toInt subgraphIndex)
        keySoundFnCf'
        (mkKeySoundFnCf graphNode (pred subgraphIndex))
