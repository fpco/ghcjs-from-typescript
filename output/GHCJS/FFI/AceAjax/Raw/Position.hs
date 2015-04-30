module GHCJS.FFI.AceAjax.Raw.Position where
import qualified GHCJS.Types as GHCJS
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import GHCJS.FFI.AceAjax.Raw.Types

foreign import javascript "$1.row" row :: Position -> GHCJS.JSNumber
foreign import javascript "$1.column" column :: Position -> GHCJS.JSNumber
