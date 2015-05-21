module JavaScript.AceAjax.Raw.Delta where
import qualified GHCJS.Types as GHCJS
import qualified GHCJS.Marshal as GHCJS
import qualified Data.Typeable
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import JavaScript.AceAjax.Raw.Types

foreign import javascript "$1.action" action :: Delta -> IO (GHCJS.JSString)
foreign import javascript "$1.range" range :: Delta -> IO (Range)
foreign import javascript "$1.text" text :: Delta -> IO (GHCJS.JSString)
foreign import javascript "$1.lines" lines :: Delta -> IO (GHCJS.JSArray (GHCJS.JSString))
