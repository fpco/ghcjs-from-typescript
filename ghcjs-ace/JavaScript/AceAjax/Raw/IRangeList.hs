module JavaScript.AceAjax.Raw.IRangeList where
import qualified GHCJS.Types as GHCJS
import qualified GHCJS.Marshal as GHCJS
import qualified Data.Typeable
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import JavaScript.AceAjax.Raw.Types

foreign import javascript "$1.ranges" ranges :: IRangeList -> IO (GHCJS.JSArray (Range))
foreign import javascript "$1.pointIndex($2,$3)" pointIndex :: (IRangeList) -> (Position) -> (GHCJS.JSNumber) -> IO (())
foreign import javascript "$1.addList($2)" addList :: (IRangeList) -> (GHCJS.JSArray (Range)) -> IO (())
foreign import javascript "$1.add($2)" add :: (IRangeList) -> (Range) -> IO (())
foreign import javascript "$1.merge()" merge :: (IRangeList) -> IO (GHCJS.JSArray (Range))
foreign import javascript "$1.substractPoint($2)" substractPoint :: (IRangeList) -> (Position) -> IO (())
