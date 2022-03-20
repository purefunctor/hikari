let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.7-20220228/packages.dhall
        sha256:585403682c9378a55da644fb2edbc74d2592d18283bc9fa3457ad79398bb55bb

let overrides =
      { arraybuffer =
        { dependencies =
          [ "arraybuffer-types"
          , "arrays"
          , "effect"
          , "float32"
          , "functions"
          , "gen"
          , "maybe"
          , "nullable"
          , "prelude"
          , "tailrec"
          , "uint"
          , "unfoldable"
          ]
        , repo =
            "https://github.com/purescript-contrib/purescript-arraybuffer.git"
        , version = "v12.0.0"
        }
      }

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
          , "row-options"
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
        , version = "v0.7.4"
        }
      , painting =
        { dependencies =
          [ "canvas"
          , "colors"
          , "console"
          , "effect"
          , "foldable-traversable"
          , "foreign-object"
          , "psci-support"
          , "web-html"
          ]
        , repo = "https://github.com/mikesol/purescript-painting.git"
        , version = "main"
        }
      , wags-lib =
        { dependencies = [ "string-parsers", "run", "halogen-css" ]
        , repo = "https://github.com/mikesol/purescript-wags-lib.git"
        , version = "v0.0.103"
        }
      , row-options =
        { dependencies = [ "record" ]
        , repo = "https://github.com/mikesol/purescript-row-options.git"
        , version = "v0.0.2"
        }
      , everythings-better-with-variants =
        { dependencies =
          [ "control"
          , "foldable-traversable"
          , "invariant"
          , "newtype"
          , "prelude"
          , "psci-support"
          , "variant"
          ]
        , repo =
            "https://github.com/mikesol/purescript-everythings-better-with-variants.git"
        , version = "v0.0.0"
        }
      , free =
        { dependencies =
          [ "catenable-lists"
          , "control"
          , "distributive"
          , "either"
          , "exists"
          , "foldable-traversable"
          , "invariant"
          , "lazy"
          , "maybe"
          , "prelude"
          , "tailrec"
          , "transformers"
          , "tuples"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/mikesol/purescript-free.git"
        , version = "master"
        }
      , typelevel-peano =
        { dependencies =
          [ "arrays"
          , "console"
          , "effect"
          , "prelude"
          , "psci-support"
          , "typelevel-prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/csicar/purescript-typelevel-peano.git"
        , version = "v1.0.1"
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
      , convertable-options =
        { dependencies = [ "console", "effect", "maybe", "record" ]
        , repo =
            "https://github.com/natefaubion/purescript-convertable-options.git"
        , version = "v1.0.0"
        }
      }

in  (upstream // overrides // additions)
  with bms = ../bms/spago.dhall as Location
