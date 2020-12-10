{ name = "todosprint"
, dependencies = [ "console", "effect", "psci-support", "halogen" ]
, packages = ./packages.dhall
, sources = [ "purescript/src/**/*.purs", "purescript/test/**/*.purs" ]
}
