module GHCJS.FromTypeScript.Munge (mungeName) where

import Data.Char (toLower, isAlpha)

--FIXME: Removing special chars can cause names to alias.
mungeName :: String -> String
mungeName = defaultForEmpty . avoidKeywords . onlyValidIdentifiers
  where
    defaultForEmpty "" = "_defaultForEmpty_"
    defaultForEmpty x = x
    onlyValidIdentifiers [] = []
    onlyValidIdentifiers (x:xs)
      | isAlpha x = toLower x : onlyValidIdentifiers' xs
      | otherwise = onlyValidIdentifiers xs
    onlyValidIdentifiers' [] = []
    onlyValidIdentifiers' (x:xs)
      | isAlpha x = x : onlyValidIdentifiers' xs
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
