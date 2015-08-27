{-# LANGUAGE StandaloneDeriving #-}

module GHCJS.FromTypeScript.Util where

import Data.Char (toUpper, toLower)
import Data.List (intercalate)
import Data.Monoid
import Data.Typeable
import Language.TypeScript
import Debug.Trace (trace)

skip :: Show a => a -> [b]
skip x = trace ("Skipping " ++ show x) []

notSupported :: (Show a, Typeable a) => a -> b
notSupported x = error $ "\n" ++ show (typeOf x) ++ " not yet supported: " ++ show x

deriving instance Eq ModuleName
deriving instance Ord ModuleName

instance Monoid ModuleName where
    mempty = ModuleName mempty
    mappend (ModuleName xs) (ModuleName ys) = ModuleName (xs ++ ys)

renderModuleName :: ModuleName -> String
renderModuleName (ModuleName xs) = intercalate "." xs

moduleNamePath :: ModuleName -> FilePath
moduleNamePath (ModuleName xs) = intercalate "/" xs ++ ".hs"

capitalize :: String -> String
capitalize [] = []
capitalize (c:xs) = toUpper c : xs

decapitalize :: String -> String
decapitalize [] = []
decapitalize (c:xs) = toLower c : xs
