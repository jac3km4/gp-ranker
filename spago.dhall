{ name = "gp-ranker"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "argonaut"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "flame"
  , "foldable-traversable"
  , "foreign-object"
  , "http-methods"
  , "integers"
  , "maybe"
  , "newtype"
  , "numbers"
  , "prelude"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
