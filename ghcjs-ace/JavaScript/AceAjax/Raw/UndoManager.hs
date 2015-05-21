module JavaScript.AceAjax.Raw.UndoManager where
import qualified GHCJS.Types as GHCJS
import qualified GHCJS.Marshal as GHCJS
import qualified Data.Typeable
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import JavaScript.AceAjax.Raw.Types

foreign import javascript "$1.execute($2)" execute :: (UndoManager) -> (GHCJS.JSRef obj0) -> IO (())
foreign import javascript "$1.undo($2)" undo :: (UndoManager) -> (GHCJS.JSBool) -> IO (Range)
foreign import javascript "$1.redo($2)" redo :: (UndoManager) -> (GHCJS.JSBool) -> IO (())
foreign import javascript "$1.reset()" reset :: (UndoManager) -> IO (())
foreign import javascript "$1.hasUndo()" hasUndo :: (UndoManager) -> IO (GHCJS.JSBool)
foreign import javascript "$1.hasRedo()" hasRedo :: (UndoManager) -> IO (GHCJS.JSBool)
