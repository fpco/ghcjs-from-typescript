module JavaScript.AceAjax.Raw.PlaceHolder where
import qualified GHCJS.Types as GHCJS
import qualified GHCJS.Marshal as GHCJS
import qualified Data.Typeable
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import JavaScript.AceAjax.Raw.Types

foreign import javascript "$1.on($2,$3)" on :: (PlaceHolder) -> (GHCJS.JSString) -> (GHCJS.JSFun ((GHCJS.JSRef obj0) -> IO (GHCJS.JSRef obj1))) -> IO (())
foreign import javascript "$1.setup()" setup :: (PlaceHolder) -> IO (())
foreign import javascript "$1.showOtherMarkers()" showOtherMarkers :: (PlaceHolder) -> IO (())
foreign import javascript "$1.hideOtherMarkers()" hideOtherMarkers :: (PlaceHolder) -> IO (())
foreign import javascript "$1.onUpdate()" onUpdate :: (PlaceHolder) -> IO (())
foreign import javascript "$1.onCursorChange()" onCursorChange :: (PlaceHolder) -> IO (())
foreign import javascript "$1.detach()" detach :: (PlaceHolder) -> IO (())
foreign import javascript "$1.cancel()" cancel :: (PlaceHolder) -> IO (())
