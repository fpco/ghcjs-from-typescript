module JavaScript.AceAjax.Raw.Annotation where
import qualified GHCJS.Types as GHCJS
import qualified GHCJS.Marshal as GHCJS
import qualified Data.Typeable
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import JavaScript.AceAjax.Raw.Types

foreign import javascript "$1.row" row :: Annotation -> IO (GHCJS.JSNumber)
foreign import javascript "$1.column" column :: Annotation -> IO (GHCJS.JSNumber)
foreign import javascript "$1.text" text :: Annotation -> IO (GHCJS.JSString)
foreign import javascript "$1.type" type_ :: Annotation -> IO (GHCJS.JSString)
