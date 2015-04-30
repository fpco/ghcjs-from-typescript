module GHCJS.FFI.AceAjax.Raw.UndoManager where
import qualified GHCJS.Types as GHCJS
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import GHCJS.FFI.AceAjax.Raw.Types

foreign import javascript "$1.execute($2)" execute :: UndoManager -> (GHCJS.JSRef obj0) -> IO (GHCJS.JSRef obj1)
foreign import javascript "$1.undo($2)" undo :: UndoManager -> (GHCJS.JSBool) -> IO (Range)
foreign import javascript "$1.redo($2)" redo :: UndoManager -> (GHCJS.JSBool) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.reset()" reset :: UndoManager -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.hasUndo()" hasUndo :: UndoManager -> IO (GHCJS.JSBool)
foreign import javascript "$1.hasRedo()" hasRedo :: UndoManager -> IO (GHCJS.JSBool)
