module GHCJS.FFI.AceAjax.Raw.BackgroundTokenizer where
import qualified GHCJS.Types as GHCJS
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import GHCJS.FFI.AceAjax.Raw.Types

foreign import javascript "$1.states" states :: BackgroundTokenizer -> GHCJS.JSArray (GHCJS.JSRef obj0)
foreign import javascript "$1.setTokenizer($2)" setTokenizer :: BackgroundTokenizer -> (Tokenizer) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.setDocument($2)" setDocument :: BackgroundTokenizer -> (Document) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.fireUpdateEvent($2,$3)" fireUpdateEvent :: BackgroundTokenizer -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.start($2)" start :: BackgroundTokenizer -> (GHCJS.JSNumber) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.stop()" stop :: BackgroundTokenizer -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.getTokens($2)" getTokens :: BackgroundTokenizer -> (GHCJS.JSNumber) -> IO (GHCJS.JSArray (TokenInfo))
foreign import javascript "$1.getState($2)" getState :: BackgroundTokenizer -> (GHCJS.JSNumber) -> IO (GHCJS.JSString)
