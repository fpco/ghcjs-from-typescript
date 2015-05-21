module JavaScript.AceAjax.Raw.TextMode where
import qualified GHCJS.Types as GHCJS
import qualified GHCJS.Marshal as GHCJS
import qualified Data.Typeable
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import JavaScript.AceAjax.Raw.Types

foreign import javascript "$1.getTokenizer()" getTokenizer :: (TextMode) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.toggleCommentLines($2,$3,$4,$5)" toggleCommentLines :: (TextMode) -> (GHCJS.JSRef obj0) -> (GHCJS.JSRef obj1) -> (GHCJS.JSRef obj2) -> (GHCJS.JSRef obj3) -> IO (())
foreign import javascript "$1.getNextLineIndent($2,$3,$4)" getNextLineIndent :: (TextMode) -> (GHCJS.JSRef obj0) -> (GHCJS.JSRef obj1) -> (GHCJS.JSRef obj2) -> IO (GHCJS.JSString)
foreign import javascript "$1.checkOutdent($2,$3,$4)" checkOutdent :: (TextMode) -> (GHCJS.JSRef obj0) -> (GHCJS.JSRef obj1) -> (GHCJS.JSRef obj2) -> IO (GHCJS.JSBool)
foreign import javascript "$1.autoOutdent($2,$3,$4)" autoOutdent :: (TextMode) -> (GHCJS.JSRef obj0) -> (GHCJS.JSRef obj1) -> (GHCJS.JSRef obj2) -> IO (())
foreign import javascript "$1.createWorker($2)" createWorker :: (TextMode) -> (GHCJS.JSRef obj0) -> IO (GHCJS.JSRef obj1)
foreign import javascript "$1.createModeDelegates($2)" createModeDelegates :: (TextMode) -> (GHCJS.JSRef obj0) -> IO (())
foreign import javascript "$1.transformAction($2,$3,$4,$5,$6)" transformAction :: (TextMode) -> (GHCJS.JSRef obj0) -> (GHCJS.JSRef obj1) -> (GHCJS.JSRef obj2) -> (GHCJS.JSRef obj3) -> (GHCJS.JSRef obj4) -> IO (GHCJS.JSRef obj5)
