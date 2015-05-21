module JavaScript.AceAjax.Raw.Document where
import qualified GHCJS.Types as GHCJS
import qualified GHCJS.Marshal as GHCJS
import qualified Data.Typeable
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import JavaScript.AceAjax.Raw.Types

foreign import javascript "$1.on($2,$3)" on :: (Document) -> (GHCJS.JSString) -> (GHCJS.JSFun ((GHCJS.JSRef obj0) -> IO (GHCJS.JSRef obj1))) -> IO (())
foreign import javascript "$1.setValue($2)" setValue :: (Document) -> (GHCJS.JSString) -> IO (())
foreign import javascript "$1.getValue()" getValue :: (Document) -> IO (GHCJS.JSString)
foreign import javascript "$1.createAnchor($2,$3)" createAnchor :: (Document) -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (())
foreign import javascript "$1.getNewLineCharacter()" getNewLineCharacter :: (Document) -> IO (GHCJS.JSString)
foreign import javascript "$1.setNewLineMode($2)" setNewLineMode :: (Document) -> (GHCJS.JSString) -> IO (())
foreign import javascript "$1.getNewLineMode()" getNewLineMode :: (Document) -> IO (GHCJS.JSString)
foreign import javascript "$1.isNewLine($2)" isNewLine :: (Document) -> (GHCJS.JSString) -> IO (GHCJS.JSBool)
foreign import javascript "$1.getLine($2)" getLine :: (Document) -> (GHCJS.JSNumber) -> IO (GHCJS.JSString)
foreign import javascript "$1.getLines($2,$3)" getLines :: (Document) -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (GHCJS.JSArray (GHCJS.JSString))
foreign import javascript "$1.getAllLines()" getAllLines :: (Document) -> IO (GHCJS.JSArray (GHCJS.JSString))
foreign import javascript "$1.getLength()" getLength :: (Document) -> IO (GHCJS.JSNumber)
foreign import javascript "$1.getTextRange($2)" getTextRange :: (Document) -> (Range) -> IO (GHCJS.JSString)
foreign import javascript "$1.insert($2,$3)" insert :: (Document) -> (Position) -> (GHCJS.JSString) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.insertLines($2,$3)" insertLines :: (Document) -> (GHCJS.JSNumber) -> (GHCJS.JSArray (GHCJS.JSString)) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.insertNewLine($2)" insertNewLine :: (Document) -> (Position) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.insertInLine($2,$3)" insertInLine :: (Document) -> (GHCJS.JSRef obj0) -> (GHCJS.JSString) -> IO (GHCJS.JSRef obj1)
foreign import javascript "$1.remove($2)" remove :: (Document) -> (Range) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.removeInLine($2,$3,$4)" removeInLine :: (Document) -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.removeLines($2,$3)" removeLines :: (Document) -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (GHCJS.JSArray (GHCJS.JSString))
foreign import javascript "$1.removeNewLine($2)" removeNewLine :: (Document) -> (GHCJS.JSNumber) -> IO (())
foreign import javascript "$1.replace($2,$3)" replace :: (Document) -> (Range) -> (GHCJS.JSString) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.applyDeltas($2)" applyDeltas :: (Document) -> (GHCJS.JSArray (Delta)) -> IO (())
foreign import javascript "$1.revertDeltas($2)" revertDeltas :: (Document) -> (GHCJS.JSArray (Delta)) -> IO (())
foreign import javascript "$1.indexToPosition($2,$3)" indexToPosition :: (Document) -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (Position)
foreign import javascript "$1.positionToIndex($2,$3)" positionToIndex :: (Document) -> (Position) -> (GHCJS.JSNumber) -> IO (GHCJS.JSNumber)
