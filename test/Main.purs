module Test.Main where

import Prelude

import Data.Set as Set
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Node.Glob.Basic (expandGlobsCwd)

main :: Effect Unit
main = launchAff_ do
  files <- expandGlobsCwd
    [ "./src/**/*.purs"
    , "./test/**/*.purs"
    ]
  log $ show (Set.size files) <> " files"
