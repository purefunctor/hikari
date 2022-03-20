{ name = "demo"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "bms"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "event"
  , "exceptions"
  , "foldable-traversable"
  , "free"
  , "identity"
  , "indexed-monad"
  , "lists"
  , "maybe"
  , "nonempty"
  , "ordered-collections"
  , "parallel"
  , "partial"
  , "prelude"
  , "record"
  , "sized-vectors"
  , "string-parsers"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel"
  , "wags"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}