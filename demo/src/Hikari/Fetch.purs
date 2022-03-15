module Hikari.Fetch where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import fetchTextImpl :: String -> Effect (Promise String)

fetchText :: String -> Aff String
fetchText = toAffE <<< fetchTextImpl
