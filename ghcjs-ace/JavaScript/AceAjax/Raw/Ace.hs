module JavaScript.AceAjax.Raw.Ace where
import qualified GHCJS.Types as GHCJS
import qualified GHCJS.Marshal as GHCJS
import qualified Data.Typeable
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import JavaScript.AceAjax.Raw.Types

foreign import javascript "$1.require($2)" require :: (Ace) -> (GHCJS.JSString) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.edit($2)" edit :: (Ace) -> (GHCJS.JSString) -> IO (Editor)
foreign import javascript "$1.edit($2)" edit1 :: (Ace) -> (HTMLElement) -> IO (Editor)
foreign import javascript "$1.createEditSession($2,$3)" createEditSession :: (Ace) -> (Document) -> (TextMode) -> IO (IEditSession)
foreign import javascript "$1.createEditSession($2,$3)" createEditSession1 :: (Ace) -> (GHCJS.JSString) -> (TextMode) -> IO (IEditSession)
