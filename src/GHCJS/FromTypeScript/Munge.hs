{-# LANGUAGE StandaloneDeriving #-}

module GHCJS.FromTypeScript.Munge
  ( mungeLowerName
  , mungeUpperName
  , capitalize
  , decapitalize
  , renderModuleName
  , moduleNamePath
  ) where

import Data.Char (toUpper, toLower, isAlpha, isNumber)
import Data.List (intercalate)
import Data.Monoid
import Data.Typeable
import Language.TypeScript

mungeLowerName :: String -> String
mungeLowerName = decapitalize . mungeName

mungeUpperName :: String -> String
mungeUpperName = capitalize . mungeName

capitalize :: String -> String
capitalize [] = []
capitalize (c:xs) = toUpper c : xs

decapitalize :: String -> String
decapitalize [] = []
decapitalize (c:xs) = toLower c : xs

--FIXME: Removing special chars can cause names to alias.
mungeName :: String -> String
mungeName = defaultForEmpty . avoidKeywords . onlyValidIdentifiers
  where
    defaultForEmpty "" = "defaultForEmpty_"
    defaultForEmpty x = x
    onlyValidIdentifiers [] = []
    onlyValidIdentifiers (x:xs)
      | isAlpha x = x : onlyValidIdentifiers' xs
      | otherwise = onlyValidIdentifiers xs
    onlyValidIdentifiers' [] = []
    onlyValidIdentifiers' (x:xs)
      | isAlpha x || isNumber x = x : onlyValidIdentifiers' xs
      | otherwise = onlyValidIdentifiers' xs

--FIXME: If both "type" and "type_" exist, then this will cause the
--first to alias the second, erroneously.
avoidKeywords :: String -> String
avoidKeywords x | x `elem` keywords = x ++ "_"
avoidKeywords x = x

-- https://wiki.haskell.org/Keywords
keywords :: [String]
keywords =
  [ "as"
  , "case"
  , "of"
  , "class"
  , "data"
  , "default"
  , "do"
  , "forall"
  , "foreign"
  , "hiding"
  , "if"
  , "then"
  , "else"
  , "import"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "in"
  , "mdo"
  , "module"
  , "newtype"
  , "proc"
  , "qualified"
  , "rec"
  , "type"
  , "where"
  ]

-- Module name utilities

renderModuleName :: ModuleName -> String
renderModuleName (ModuleName xs) = intercalate "." (map mungeUpperName xs)

moduleNamePath :: ModuleName -> FilePath
moduleNamePath (ModuleName xs) = intercalate "/" (map mungeUpperName xs) ++ ".hs"

deriving instance Eq ModuleName
deriving instance Ord ModuleName

instance Monoid ModuleName where
    mempty = ModuleName mempty
    mappend (ModuleName xs) (ModuleName ys) = ModuleName (xs ++ ys)
