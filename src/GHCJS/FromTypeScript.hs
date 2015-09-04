module GHCJS.FromTypeScript where

import GHCJS.FromTypeScript.Collect (collect)
import GHCJS.FromTypeScript.Munge
import GHCJS.FromTypeScript.Render (render)
import GHCJS.FromTypeScript.Types
import GHCJS.FromTypeScript.Util
import Language.TypeScript
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Text.Parsec (parse)

ghcjsFromTypeScript :: Config -> FilePath -> IO [ModuleName]
ghcjsFromTypeScript config fp = do
  parsed <- parseTypeScript fp
  let outputs = render (collect parsed)
  mapM_ (writeOutput config) outputs
  return (map fst outputs)

parseTypeScript :: FilePath -> IO [DeclarationElement]
parseTypeScript fp =
  either (fail . show) return .
  parse declarationSourceFile fp =<<
  readFile fp

writeOutput :: Config -> (ModuleName, OutputModule) -> IO ()
writeOutput config (mn, OutputModule imports contents) = do
   let fp = outputDir config </> moduleNamePath mn
   createDirectoryIfMissing True (takeDirectory fp)
   writeFile fp $ unlines $
     pragmas ++
     ["module " ++ renderModuleName mn ++ " where"] ++
     imports ++
     contents

pragmas :: [String]
pragmas =
  [ "{-# LANGUAGE ConstraintKinds            #-}"
  , "{-# LANGUAGE DataKinds                  #-}"
  , "{-# LANGUAGE FlexibleContexts           #-}"
  , "{-# LANGUAGE FlexibleInstances          #-}"
  , "{-# LANGUAGE TypeFamilies               #-}"
  , "{-# LANGUAGE TypeOperators              #-}"
  , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
  , "{-# LANGUAGE PolyKinds                  #-}"
  , "{-# LANGUAGE UndecidableInstances       #-}"
  , "{-# LANGUAGE RankNTypes                 #-}"
  ]

defaultConfig :: FilePath -> Config
defaultConfig out = Config
  { outputDir = out
  }
