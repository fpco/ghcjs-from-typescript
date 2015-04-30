module GHCJS.FFI.AceAjax.Raw.Split where
import qualified GHCJS.Types as GHCJS
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import GHCJS.FFI.AceAjax.Raw.Types

foreign import javascript "$1.getSplits()" getSplits :: Split -> IO (GHCJS.JSNumber)
foreign import javascript "$1.getEditor($2)" getEditor :: Split -> (GHCJS.JSNumber) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.getCurrentEditor()" getCurrentEditor :: Split -> IO (Editor)
foreign import javascript "$1.focus()" focus :: Split -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.blur()" blur :: Split -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.setTheme($2)" setTheme :: Split -> (GHCJS.JSString) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.setKeyboardHandler($2)" setKeyboardHandler :: Split -> (GHCJS.JSString) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.forEach($2,$3)" forEach :: Split -> (Function) -> (GHCJS.JSString) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.setFontSize($2)" setFontSize :: Split -> (GHCJS.JSNumber) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.setSession($2,$3)" setSession :: Split -> (IEditSession) -> (GHCJS.JSNumber) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.getOrientation()" getOrientation :: Split -> IO (GHCJS.JSNumber)
foreign import javascript "$1.setOrientation($2)" setOrientation :: Split -> (GHCJS.JSNumber) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.resize()" resize :: Split -> IO (GHCJS.JSRef obj0)
