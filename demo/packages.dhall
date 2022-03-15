let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.7-20220303/packages.dhall
        sha256:d7cbc15ea16768e4a4f99baa58a54559dd2648c6c1362de2469d9e41c23b28c3

let additions =
      { wags =
        { dependencies =
          [ "aff"
          , "aff-promise"
          , "arraybuffer-types"
          , "behaviors"
          , "control"
          , "convertable-options"
          , "datetime"
          , "effect"
          , "either"
          , "event"
          , "everythings-better-with-variants"
          , "foldable-traversable"
          , "foreign"
          , "foreign-object"
          , "free"
          , "heterogeneous"
          , "indexed-monad"
          , "integers"
          , "js-timers"
          , "lists"
          , "math"
          , "maybe"
          , "newtype"
          , "nullable"
          , "ordered-collections"
          , "prelude"
          , "psci-support"
          , "record"
          , "refs"
          , "safe-coerce"
          , "sized-vectors"
          , "tuples"
          , "typelevel"
          , "typelevel-peano"
          , "unsafe-coerce"
          , "untagged-union"
          , "web-events"
          , "simple-json"
          ]
        , repo = "https://github.com/mikesol/purescript-wags.git"
        , version = "v0.7.0"
        }
      , wags-lib =
        { dependencies = [ "homogeneous", "string-parsers", "run", "halogen-css" ]
        , repo = "https://github.com/mikesol/purescript-wags-lib.git"
        , version = "v0.0.100"
        }
      , behaviors =
        { dependencies =
          [ "psci-support"
          , "effect"
          , "ordered-collections"
          , "filterable"
          , "nullable"
          , "event"
          , "web-html"
          , "web-events"
          , "web-uievents"
          ]
        , repo = "https://github.com/mikesol/purescript-behaviors.git"
        , version = "v8.1.0"
        }
      , event =
        { dependencies =
          [ "console"
          , "effect"
          , "filterable"
          , "nullable"
          , "unsafe-reference"
          , "js-timers"
          , "now"
          ]
        , repo = "https://github.com/mikesol/purescript-event.git"
        , version = "v1.4.2"
        }
      , everythings-better-with-variants =
        { dependencies =
          [ "control"
          , "foldable-traversable"
          , "invariant"
          , "newtype"
          , "prelude"
          , "variant"
          ]
        , repo =
            "https://github.com/mikesol/purescript-everythings-better-with-variants.git"
        , version = "v0.0.0"
        }
      }

in  (upstream // additions)
  with bms = ../bms/spago.dhall as Location
