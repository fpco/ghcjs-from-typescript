module GHCJS.FFI.AceAjax.Raw.Anchor where
import qualified GHCJS.Types as GHCJS
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import GHCJS.FFI.AceAjax.Raw.Types

foreign import javascript "$1.on($2,$3)" on :: Anchor -> (GHCJS.JSString) -> (GHCJS.JSRef ((GHCJS.JSRef obj0) -> IO (GHCJS.JSRef obj1))) -> IO (GHCJS.JSRef obj2)
foreign import javascript "$1.getPosition()" getPosition :: Anchor -> IO (Position)
foreign import javascript "$1.getDocument()" getDocument :: Anchor -> IO (Document)
foreign import javascript "$1.onChange($2)" onChange :: Anchor -> (GHCJS.JSRef obj0) -> IO (GHCJS.JSRef obj1)
foreign import javascript "$1.setPosition($2,$3,$4)" setPosition :: Anchor -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> (GHCJS.JSBool) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.detach()" detach :: Anchor -> IO (GHCJS.JSRef obj0)
