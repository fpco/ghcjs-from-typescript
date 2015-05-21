module JavaScript.AceAjax.Raw.ScrollBar where
import qualified GHCJS.Types as GHCJS
import qualified GHCJS.Marshal as GHCJS
import qualified Data.Typeable
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import JavaScript.AceAjax.Raw.Types

foreign import javascript "$1.onScroll($2)" onScroll :: (ScrollBar) -> (GHCJS.JSRef obj0) -> IO (())
foreign import javascript "$1.getWidth()" getWidth :: (ScrollBar) -> IO (GHCJS.JSNumber)
foreign import javascript "$1.setHeight($2)" setHeight :: (ScrollBar) -> (GHCJS.JSNumber) -> IO (())
foreign import javascript "$1.setInnerHeight($2)" setInnerHeight :: (ScrollBar) -> (GHCJS.JSNumber) -> IO (())
foreign import javascript "$1.setScrollTop($2)" setScrollTop :: (ScrollBar) -> (GHCJS.JSNumber) -> IO (())
