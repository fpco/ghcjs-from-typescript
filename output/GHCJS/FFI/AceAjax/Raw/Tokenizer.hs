module GHCJS.FFI.AceAjax.Raw.Tokenizer where
import qualified GHCJS.Types as GHCJS
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import GHCJS.FFI.AceAjax.Raw.Types

foreign import javascript "$1.getLineTokens()" getLineTokens :: Tokenizer -> IO (GHCJS.JSRef obj0)
