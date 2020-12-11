{ name = "todosprint"
, dependencies =
    [ "console"
    , "effect"
    , "psci-support"
    , "halogen"
    , "affjax"
    , "argonaut"
    , "argonaut-generic"
    , "formatters"
    , "strings"
    ]
, packages = ./packages.dhall
, sources = [ "purescript/src/**/*.purs", "purescript/test/**/*.purs" ]
}
