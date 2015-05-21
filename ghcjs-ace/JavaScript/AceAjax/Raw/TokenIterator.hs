module JavaScript.AceAjax.Raw.TokenIterator where
import qualified GHCJS.Types as GHCJS
import qualified GHCJS.Marshal as GHCJS
import qualified Data.Typeable
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import JavaScript.AceAjax.Raw.Types

foreign import javascript "$1.stepBackward()" stepBackward :: (TokenIterator) -> IO (GHCJS.JSArray (GHCJS.JSString))
foreign import javascript "$1.stepForward()" stepForward :: (TokenIterator) -> IO (GHCJS.JSString)
foreign import javascript "$1.getCurrentToken()" getCurrentToken :: (TokenIterator) -> IO (TokenInfo)
foreign import javascript "$1.getCurrentTokenRow()" getCurrentTokenRow :: (TokenIterator) -> IO (GHCJS.JSNumber)
foreign import javascript "$1.getCurrentTokenColumn()" getCurrentTokenColumn :: (TokenIterator) -> IO (GHCJS.JSNumber)
