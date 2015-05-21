module JavaScript.AceAjax.Raw.TokenInfo where
import qualified GHCJS.Types as GHCJS
import qualified GHCJS.Marshal as GHCJS
import qualified Data.Typeable
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import JavaScript.AceAjax.Raw.Types

foreign import javascript "$1.value" value :: TokenInfo -> IO (GHCJS.JSString)
