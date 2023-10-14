let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20231014/packages.dhall
        sha256:779d9425686b00140c0bb94b9f5b1eab052643d36e30b900c7fe7d45bab315d5

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
