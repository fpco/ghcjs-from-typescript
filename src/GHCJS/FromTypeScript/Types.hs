module GHCJS.FromTypeScript.Types where

import Language.TypeScript
import Data.Monoid

data Config = Config
  { outputDir :: FilePath
  }

data Decl
  = InterfaceDecl Interface
  deriving (Show)

data OutputModule = OutputModule
  { omImports :: [String]
  , omDecls :: [String]
  } deriving (Show)

instance Monoid OutputModule where
  mempty = OutputModule [] []
  mappend (OutputModule imports1 decls1)
          (OutputModule imports2 decls2) =
    OutputModule (imports1 <> imports2)
                 (decls1 <> decls2)
