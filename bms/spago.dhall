{ name = "bms"
, dependencies =
  [ "aff"
  , "effect"
  , "foldable-traversable"
  , "lists"
  , "newtype"
  , "nonempty"
  , "ordered-collections"
  , "prelude"
  , "spec"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
