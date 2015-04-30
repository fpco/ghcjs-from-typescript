module GHCJS.FFI.AceAjax.Raw.TokenIterator where
import qualified GHCJS.Types as GHCJS
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import GHCJS.FFI.AceAjax.Raw.Types

foreign import javascript "$1.stepBackward()" stepBackward :: TokenIterator -> IO (GHCJS.JSArray (GHCJS.JSString))
foreign import javascript "$1.stepForward()" stepForward :: TokenIterator -> IO (GHCJS.JSString)
foreign import javascript "$1.getCurrentToken()" getCurrentToken :: TokenIterator -> IO (TokenInfo)
foreign import javascript "$1.getCurrentTokenRow()" getCurrentTokenRow :: TokenIterator -> IO (GHCJS.JSNumber)
foreign import javascript "$1.getCurrentTokenColumn()" getCurrentTokenColumn :: TokenIterator -> IO (GHCJS.JSNumber)
