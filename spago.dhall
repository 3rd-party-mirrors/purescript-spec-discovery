{ name = "spec-discovery"

, packages = ./packages.dhall

, dependencies =
    [ "arrays"
    , "console"
    , "effect"
    , "node-fs"
    , "prelude"
    , "spec"
    -----------------------------
    -- dev dependencies ---------
    , "psci-support"
    ]

, sources =
    [ "src/**/*.purs"
    , "test/**/*.purs"
    ]
}
