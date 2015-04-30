module GHCJS.FFI.AceAjax.Raw.ScrollBar where
import qualified GHCJS.Types as GHCJS
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import GHCJS.FFI.AceAjax.Raw.Types

foreign import javascript "$1.onScroll($2)" onScroll :: ScrollBar -> (GHCJS.JSRef obj0) -> IO (GHCJS.JSRef obj1)
foreign import javascript "$1.getWidth()" getWidth :: ScrollBar -> IO (GHCJS.JSNumber)
foreign import javascript "$1.setHeight($2)" setHeight :: ScrollBar -> (GHCJS.JSNumber) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.setInnerHeight($2)" setInnerHeight :: ScrollBar -> (GHCJS.JSNumber) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.setScrollTop($2)" setScrollTop :: ScrollBar -> (GHCJS.JSNumber) -> IO (GHCJS.JSRef obj0)
