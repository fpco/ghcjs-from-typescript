module GHCJS.FromTypeScript.Util where

import Data.Typeable
import Debug.Trace (trace)

skip :: Show a => a -> [b]
skip x = trace ("Skipping " ++ show x) []

notSupported :: (Show a, Typeable a) => a -> b
notSupported x = error $ "\n" ++ show (typeOf x) ++ " not yet supported: " ++ show x
