module Main where

import Control.Exception (try, SomeException)
import Control.Monad (when)
import Data.Foldable (forM_)
import Data.List (isSuffixOf)
import Data.List (stripPrefix)
import GHCJS.FromTypeScript
import GHCJS.FromTypeScript.Munge (renderModuleName)
import Language.TypeScript (ModuleName)
import System.Directory
import System.FilePath ((</>), (<.>), takeFileName)

main :: IO ()
main = do
  -- Convert type-script lib
  convert "deps/lib.d.ts"
  -- Convert all of the definitely typed definition files.
  let root = "deps/DefinitelyTyped"
  dirs <- getDirectoryContents root
  forM_ dirs $ \dir -> do
    let path = root </> dir
    isDir <- doesDirectoryExist path
    when (isDir && dir `notElem` expectedFailures) $ do
      files <- getDirectoryContents path
      forM_ files $ \file -> do
        let path' = path </> file
        isFile <- doesFileExist path'
        when isFile (convert path')

convert :: FilePath -> IO ()
convert path = do
  forM_ (stripSuffix ".d.ts" (takeFileName path)) $ \name -> do
    putStrLn ""
    putStrLn path
    putStrLn ""
    let outputDir = "test/output" </> name
    eres <- try $ ghcjsFromTypeScript (defaultConfig outputDir) path
    print (eres :: Either SomeException [ModuleName])
    case eres of
      Left _ -> return ()
      Right [] -> putStrLn "Warning: no output modules"
      Right modules -> do
        writeCabalFile outputDir name modules
        writeStackFile outputDir

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

writeCabalFile :: FilePath -> String -> [ModuleName] -> IO ()
writeCabalFile dir name modules =
  writeFile (dir </> name <.> "cabal") $
    "name:                " ++ name ++ "\n\
    \version:             0.0.0\n\
    \synopsis:            Generated from https://github.com/borisyankov/DefinitelyTyped\n\
    \description:\n\
    \license:             BSD3\n\
    \license-file:        LICENSE\n\
    \category:            Web\n\
    \build-type:          Simple\n\
    \cabal-version:       >=1.10\n\
    \\n\
    \library\n\
    \  exposed-modules:" ++
    concatMap (('\n':replicate 22 ' ') ++) (map renderModuleName modules) ++ "\n" ++
    "  build-depends:      base >= 4 && < 5,\n\
    \                      ghcjs-base,\n\
    \                      ghcjs-typescript\n\
    \  default-language:   Haskell2010\n\
    \  default-extensions: DeriveDataTypeable\n\
    \                      ForeignFunctionInterface\n\
    \                      GeneralizedNewtypeDeriving\n\
    \                      JavaScriptFFI\n"

writeStackFile :: FilePath -> IO ()
writeStackFile fp = do
  writeFile (fp </> "stack.yaml") $
    "resolver: ghcjs-0.1.0_ghc-7.10.2\n\
    \extra-deps:\n\
    \- hvect-0.2.0.0\n\
    \packages:\n\
    \- .\n\
    \- ../../../../ghcjs-typescript"

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix xs = fmap reverse (stripPrefix (reverse suffix) (reverse xs))
