module GHCJS.FFI.AceAjax.Raw.Search where
import qualified GHCJS.Types as GHCJS
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import GHCJS.FFI.AceAjax.Raw.Types

foreign import javascript "$1.set($2)" set :: Search -> (GHCJS.JSRef obj0) -> IO (Search)
foreign import javascript "$1.getOptions()" getOptions :: Search -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.setOptions($2)" setOptions :: Search -> (GHCJS.JSRef obj0) -> IO (GHCJS.JSRef obj1)
foreign import javascript "$1.find($2)" find :: Search -> (IEditSession) -> IO (Range)
foreign import javascript "$1.findAll($2)" findAll :: Search -> (IEditSession) -> IO (GHCJS.JSArray (Range))
foreign import javascript "$1.replace($2,$3)" replace :: Search -> (GHCJS.JSString) -> (GHCJS.JSString) -> IO (GHCJS.JSString)
