module JavaScript.AceAjax.Raw.EditorCommand where
import qualified GHCJS.Types as GHCJS
import qualified GHCJS.Marshal as GHCJS
import qualified Data.Typeable
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import JavaScript.AceAjax.Raw.Types

foreign import javascript "$1.name" name :: EditorCommand -> IO (GHCJS.JSString)
foreign import javascript "$1.bindKey" bindKey :: EditorCommand -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.exec" exec :: EditorCommand -> IO (Function)
