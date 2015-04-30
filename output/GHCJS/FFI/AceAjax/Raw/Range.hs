module GHCJS.FFI.AceAjax.Raw.Range where
import qualified GHCJS.Types as GHCJS
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import GHCJS.FFI.AceAjax.Raw.Types

foreign import javascript "$1.startRow" startRow :: Range -> GHCJS.JSNumber
foreign import javascript "$1.startColumn" startColumn :: Range -> GHCJS.JSNumber
foreign import javascript "$1.endRow" endRow :: Range -> GHCJS.JSNumber
foreign import javascript "$1.endColumn" endColumn :: Range -> GHCJS.JSNumber
foreign import javascript "$1.start" start :: Range -> Position
foreign import javascript "$1.end" end :: Range -> Position
foreign import javascript "$1.isEmpty()" isEmpty :: Range -> IO (GHCJS.JSBool)
foreign import javascript "$1.isEqual($2)" isEqual :: Range -> (Range) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.toString()" toString :: Range -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.contains($2,$3)" contains :: Range -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (GHCJS.JSBool)
foreign import javascript "$1.compareRange($2)" compareRange :: Range -> (Range) -> IO (GHCJS.JSNumber)
foreign import javascript "$1.comparePoint($2)" comparePoint :: Range -> (Range) -> IO (GHCJS.JSNumber)
foreign import javascript "$1.containsRange($2)" containsRange :: Range -> (Range) -> IO (GHCJS.JSBool)
foreign import javascript "$1.intersects($2)" intersects :: Range -> (Range) -> IO (GHCJS.JSBool)
foreign import javascript "$1.isEnd($2,$3)" isEnd :: Range -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (GHCJS.JSBool)
foreign import javascript "$1.isStart($2,$3)" isStart :: Range -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (GHCJS.JSBool)
foreign import javascript "$1.setStart($2,$3)" setStart :: Range -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.setEnd($2,$3)" setEnd :: Range -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.inside($2,$3)" inside :: Range -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (GHCJS.JSBool)
foreign import javascript "$1.insideStart($2,$3)" insideStart :: Range -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (GHCJS.JSBool)
foreign import javascript "$1.insideEnd($2,$3)" insideEnd :: Range -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (GHCJS.JSBool)
foreign import javascript "$1.compare($2,$3)" compare :: Range -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (GHCJS.JSNumber)
foreign import javascript "$1.compareStart($2,$3)" compareStart :: Range -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (GHCJS.JSNumber)
foreign import javascript "$1.compareEnd($2,$3)" compareEnd :: Range -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (GHCJS.JSNumber)
foreign import javascript "$1.compareInside($2,$3)" compareInside :: Range -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (GHCJS.JSNumber)
foreign import javascript "$1.clipRows($2,$3)" clipRows :: Range -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (Range)
foreign import javascript "$1.extend($2,$3)" extend :: Range -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (Range)
foreign import javascript "$1.isMultiLine()" isMultiLine :: Range -> IO (GHCJS.JSBool)
foreign import javascript "$1.clone()" clone :: Range -> IO (Range)
foreign import javascript "$1.collapseRows()" collapseRows :: Range -> IO (Range)
foreign import javascript "$1.toScreenRange($2)" toScreenRange :: Range -> (IEditSession) -> IO (Range)
foreign import javascript "$1.fromPoints($2,$3)" fromPoints :: Range -> (Range) -> (Range) -> IO (Range)
