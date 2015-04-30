module GHCJS.FFI.AceAjax.Raw.CommandManager where
import qualified GHCJS.Types as GHCJS
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import GHCJS.FFI.AceAjax.Raw.Types

foreign import javascript "$1.byName" byName :: CommandManager -> GHCJS.JSRef obj0
foreign import javascript "$1.commands" commands :: CommandManager -> GHCJS.JSRef obj0
foreign import javascript "$1.platform" platform :: CommandManager -> GHCJS.JSString
foreign import javascript "$1.addCommands($2)" addCommands :: CommandManager -> (GHCJS.JSArray (EditorCommand)) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.addCommand($2)" addCommand :: CommandManager -> (EditorCommand) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.exec($2,$3,$4)" exec :: CommandManager -> (GHCJS.JSString) -> (Editor) -> (GHCJS.JSRef obj0) -> IO (GHCJS.JSRef obj1)
