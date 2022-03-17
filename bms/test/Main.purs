module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Parser (testParser)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Timing (testTiming)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  testParser
  testTiming
