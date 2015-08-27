module Main where

import GHCJS.FromTypeScript
import System.Directory
import Control.Monad

main = do
  let outputDir = "ghcjs-highcharts"
  exists <- doesDirectoryExist outputDir
  when exists $ removeDirectoryRecursive outputDir
  ghcjsFromTypeScript (defaultConfig outputDir) "highcharts.d.ts"
