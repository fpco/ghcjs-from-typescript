module GHCJS.FFI.AceAjax.Raw.Delta where
import qualified GHCJS.Types as GHCJS
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import GHCJS.FFI.AceAjax.Raw.Types

foreign import javascript "$1.action" action :: Delta -> GHCJS.JSString
foreign import javascript "$1.range" range :: Delta -> Range
foreign import javascript "$1.text" text :: Delta -> GHCJS.JSString
foreign import javascript "$1.lines" lines :: Delta -> GHCJS.JSArray (GHCJS.JSString)
