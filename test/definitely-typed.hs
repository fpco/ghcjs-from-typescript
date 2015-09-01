module Main where

import Control.Monad (when)
import Data.Foldable (forM_)
import Data.List (isSuffixOf)
import GHCJS.FromTypeScript
import System.Directory
import System.FilePath ((</>))
import Control.Exception (try, SomeException)

main :: IO ()
main = do
  let root = "test/DefinitelyTyped"
  dirs <- getDirectoryContents root
  forM_ dirs $ \dir -> do
    let path = root </> dir
    isDir <- doesDirectoryExist path
    when (isDir && dir `notElem` expectedFailures) $ do
      files <- getDirectoryContents path
      forM_ files $ \file -> do
        let path' = path </> file
        isFile <- doesFileExist path'
        when (isFile && (".d.ts" `isSuffixOf` path')) $ do
          putStrLn ""
          putStrLn path'
          putStrLn ""
          eres <- try $ ghcjsFromTypeScript (defaultConfig ("test/output" </> file)) path'
          print (eres :: Either SomeException ())

expectedFailures :: [String]
expectedFailures =
  -- Invalid - uses 'namespace' declaration
  [ "knex"
  , "node-slack"
  , "yamljs"
  , "vinyl-paths"
  , "vinyl-buffer"
  , "gulp-coffeeify"
  , "jquery.ajaxfile"
  , "node-notifier"
  -- "import * as", not yet documented in spec
  -- https://github.com/Microsoft/TypeScript/issues/2242#issuecomment-83694181
  , "node"
  , "serve-static"
  , "sequelize-fixtures"
  , "body-parser"
  , "bunyan"
  -- Invalid - uses ',' instead of ';'?!?!
  , "ngkookies"
  , "postal"
  , "snapsvg"
  , "jquery.colorpicker"
  -- Use of 'const' on a non-enum
  , "gulp-newer"
  , "gulp-cached"
  ]

-- To revisit:
--   * core-js.d.ts  (Using . qualification in type reference)
--   * umbraco.d.ts multiplexjs.d.ts (top level import)
