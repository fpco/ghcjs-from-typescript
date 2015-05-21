module JavaScript.AceAjax.Raw.KeyBinding where
import qualified GHCJS.Types as GHCJS
import qualified GHCJS.Marshal as GHCJS
import qualified Data.Typeable
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import JavaScript.AceAjax.Raw.Types

foreign import javascript "$1.setDefaultHandler($2)" setDefaultHandler :: (KeyBinding) -> (GHCJS.JSRef obj0) -> IO (())
foreign import javascript "$1.setKeyboardHandler($2)" setKeyboardHandler :: (KeyBinding) -> (GHCJS.JSRef obj0) -> IO (())
foreign import javascript "$1.addKeyboardHandler($2,$3)" addKeyboardHandler :: (KeyBinding) -> (GHCJS.JSRef obj0) -> (GHCJS.JSRef obj1) -> IO (())
foreign import javascript "$1.removeKeyboardHandler($2)" removeKeyboardHandler :: (KeyBinding) -> (GHCJS.JSRef obj0) -> IO (GHCJS.JSBool)
foreign import javascript "$1.getKeyboardHandler()" getKeyboardHandler :: (KeyBinding) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.onCommandKey($2,$3,$4)" onCommandKey :: (KeyBinding) -> (GHCJS.JSRef obj0) -> (GHCJS.JSRef obj1) -> (GHCJS.JSRef obj2) -> IO (())
foreign import javascript "$1.onTextInput($2)" onTextInput :: (KeyBinding) -> (GHCJS.JSRef obj0) -> IO (())
