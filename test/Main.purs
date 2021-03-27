module Test.Main where

import Prelude

import Data.FoldableWithIndex (forWithIndex_)
import Data.Map as Map
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Node.Glob.Basic (expandGlobsWithStatsCwd)

main :: Effect Unit
main = launchAff_ do
  files <- expandGlobsWithStatsCwd
    [ "./src/**/*.purs"
    , "./test/**/*.purs"
    , ".spago/*/*/src/**/*.purs"
    ]
  forWithIndex_ files \path _ -> log path
  log $ show (Map.size files) <> " files"
