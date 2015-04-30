module GHCJS.FFI.AceAjax.Raw.Annotation where
import qualified GHCJS.Types as GHCJS
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import GHCJS.FFI.AceAjax.Raw.Types

foreign import javascript "$1.row" row :: Annotation -> GHCJS.JSNumber
foreign import javascript "$1.column" column :: Annotation -> GHCJS.JSNumber
foreign import javascript "$1.text" text :: Annotation -> GHCJS.JSString
foreign import javascript "$1.type" type_ :: Annotation -> GHCJS.JSString
