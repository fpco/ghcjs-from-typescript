module GHCJS.FFI.AceAjax.Raw.EditorCommand where
import qualified GHCJS.Types as GHCJS
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import GHCJS.FFI.AceAjax.Raw.Types

foreign import javascript "$1.name" name :: EditorCommand -> GHCJS.JSString
foreign import javascript "$1.bindKey" bindKey :: EditorCommand -> GHCJS.JSRef obj0
foreign import javascript "$1.exec" exec :: EditorCommand -> Function
