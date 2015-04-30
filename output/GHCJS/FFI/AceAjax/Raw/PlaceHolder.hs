module GHCJS.FFI.AceAjax.Raw.PlaceHolder where
import qualified GHCJS.Types as GHCJS
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import GHCJS.FFI.AceAjax.Raw.Types

foreign import javascript "$1.on($2,$3)" on :: PlaceHolder -> (GHCJS.JSString) -> (GHCJS.JSRef ((GHCJS.JSRef obj0) -> IO (GHCJS.JSRef obj1))) -> IO (GHCJS.JSRef obj2)
foreign import javascript "$1.setup()" setup :: PlaceHolder -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.showOtherMarkers()" showOtherMarkers :: PlaceHolder -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.hideOtherMarkers()" hideOtherMarkers :: PlaceHolder -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.onUpdate()" onUpdate :: PlaceHolder -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.onCursorChange()" onCursorChange :: PlaceHolder -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.detach()" detach :: PlaceHolder -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.cancel()" cancel :: PlaceHolder -> IO (GHCJS.JSRef obj0)
