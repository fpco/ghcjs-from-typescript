module JavaScript.AceAjax.Raw.Tokenizer where
import qualified GHCJS.Types as GHCJS
import qualified GHCJS.Marshal as GHCJS
import qualified Data.Typeable
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import JavaScript.AceAjax.Raw.Types

foreign import javascript "$1.getLineTokens()" getLineTokens :: (Tokenizer) -> IO (GHCJS.JSRef obj0)
