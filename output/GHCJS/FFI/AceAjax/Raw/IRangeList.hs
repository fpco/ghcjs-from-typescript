module GHCJS.FFI.AceAjax.Raw.IRangeList where
import qualified GHCJS.Types as GHCJS
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import GHCJS.FFI.AceAjax.Raw.Types

foreign import javascript "$1.ranges" ranges :: IRangeList -> GHCJS.JSArray (Range)
foreign import javascript "$1.pointIndex($2,$3)" pointIndex :: IRangeList -> (Position) -> (GHCJS.JSNumber) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.addList($2)" addList :: IRangeList -> (GHCJS.JSArray (Range)) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.add($2)" add :: IRangeList -> (Range) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.merge()" merge :: IRangeList -> IO (GHCJS.JSArray (Range))
foreign import javascript "$1.substractPoint($2)" substractPoint :: IRangeList -> (Position) -> IO (GHCJS.JSRef obj0)
