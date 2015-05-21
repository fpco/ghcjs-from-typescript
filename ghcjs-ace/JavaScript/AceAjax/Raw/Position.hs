module JavaScript.AceAjax.Raw.Position where
import qualified GHCJS.Types as GHCJS
import qualified GHCJS.Marshal as GHCJS
import qualified Data.Typeable
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import JavaScript.AceAjax.Raw.Types

foreign import javascript "$1.row" row :: Position -> IO (GHCJS.JSNumber)
foreign import javascript "$1.column" column :: Position -> IO (GHCJS.JSNumber)
