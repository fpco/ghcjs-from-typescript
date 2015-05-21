module JavaScript.AceAjax.Raw.CommandManager where
import qualified GHCJS.Types as GHCJS
import qualified GHCJS.Marshal as GHCJS
import qualified Data.Typeable
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import JavaScript.AceAjax.Raw.Types

foreign import javascript "$1.byName" byName :: CommandManager -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.commands" commands :: CommandManager -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.platform" platform :: CommandManager -> IO (GHCJS.JSString)
foreign import javascript "$1.addCommands($2)" addCommands :: (CommandManager) -> (GHCJS.JSArray (EditorCommand)) -> IO (())
foreign import javascript "$1.addCommand($2)" addCommand :: (CommandManager) -> (EditorCommand) -> IO (())
foreign import javascript "$1.exec($2,$3,$4)" exec :: (CommandManager) -> (GHCJS.JSString) -> (Editor) -> (GHCJS.JSRef obj0) -> IO (())
