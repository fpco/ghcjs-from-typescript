module JavaScript.AceAjax.Raw.VirtualRenderer where
import qualified GHCJS.Types as GHCJS
import qualified GHCJS.Marshal as GHCJS
import qualified Data.Typeable
import GHCJS.FFI.TypeScript
import GHCJS.DOM.Types (HTMLElement)
import JavaScript.AceAjax.Raw.Types

foreign import javascript "$1.scroller" scroller :: VirtualRenderer -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.characterWidth" characterWidth :: VirtualRenderer -> IO (GHCJS.JSNumber)
foreign import javascript "$1.lineHeight" lineHeight :: VirtualRenderer -> IO (GHCJS.JSNumber)
foreign import javascript "$1.screenToTextCoordinates($2,$3)" screenToTextCoordinates :: (VirtualRenderer) -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (())
foreign import javascript "$1.setSession($2)" setSession :: (VirtualRenderer) -> (IEditSession) -> IO (())
foreign import javascript "$1.updateLines($2,$3)" updateLines :: (VirtualRenderer) -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (())
foreign import javascript "$1.updateText()" updateText :: (VirtualRenderer) -> IO (())
foreign import javascript "$1.updateFull($2)" updateFull :: (VirtualRenderer) -> (GHCJS.JSBool) -> IO (())
foreign import javascript "$1.updateFontSize()" updateFontSize :: (VirtualRenderer) -> IO (())
foreign import javascript "$1.onResize($2,$3,$4,$5)" onResize :: (VirtualRenderer) -> (GHCJS.JSBool) -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (())
foreign import javascript "$1.adjustWrapLimit()" adjustWrapLimit :: (VirtualRenderer) -> IO (())
foreign import javascript "$1.setAnimatedScroll($2)" setAnimatedScroll :: (VirtualRenderer) -> (GHCJS.JSBool) -> IO (())
foreign import javascript "$1.getAnimatedScroll()" getAnimatedScroll :: (VirtualRenderer) -> IO (GHCJS.JSBool)
foreign import javascript "$1.setShowInvisibles($2)" setShowInvisibles :: (VirtualRenderer) -> (GHCJS.JSBool) -> IO (())
foreign import javascript "$1.getShowInvisibles()" getShowInvisibles :: (VirtualRenderer) -> IO (GHCJS.JSBool)
foreign import javascript "$1.setShowPrintMargin($2)" setShowPrintMargin :: (VirtualRenderer) -> (GHCJS.JSBool) -> IO (())
foreign import javascript "$1.getShowPrintMargin()" getShowPrintMargin :: (VirtualRenderer) -> IO (GHCJS.JSBool)
foreign import javascript "$1.setPrintMarginColumn($2)" setPrintMarginColumn :: (VirtualRenderer) -> (GHCJS.JSBool) -> IO (())
foreign import javascript "$1.getPrintMarginColumn()" getPrintMarginColumn :: (VirtualRenderer) -> IO (GHCJS.JSBool)
foreign import javascript "$1.getShowGutter()" getShowGutter :: (VirtualRenderer) -> IO (GHCJS.JSBool)
foreign import javascript "$1.setShowGutter($2)" setShowGutter :: (VirtualRenderer) -> (GHCJS.JSBool) -> IO (())
foreign import javascript "$1.getContainerElement()" getContainerElement :: (VirtualRenderer) -> IO (HTMLElement)
foreign import javascript "$1.getMouseEventTarget()" getMouseEventTarget :: (VirtualRenderer) -> IO (HTMLElement)
foreign import javascript "$1.getTextAreaContainer()" getTextAreaContainer :: (VirtualRenderer) -> IO (HTMLElement)
foreign import javascript "$1.getFirstVisibleRow()" getFirstVisibleRow :: (VirtualRenderer) -> IO (GHCJS.JSNumber)
foreign import javascript "$1.getFirstFullyVisibleRow()" getFirstFullyVisibleRow :: (VirtualRenderer) -> IO (GHCJS.JSNumber)
foreign import javascript "$1.getLastFullyVisibleRow()" getLastFullyVisibleRow :: (VirtualRenderer) -> IO (GHCJS.JSNumber)
foreign import javascript "$1.getLastVisibleRow()" getLastVisibleRow :: (VirtualRenderer) -> IO (GHCJS.JSNumber)
foreign import javascript "$1.setPadding($2)" setPadding :: (VirtualRenderer) -> (GHCJS.JSNumber) -> IO (())
foreign import javascript "$1.getHScrollBarAlwaysVisible()" getHScrollBarAlwaysVisible :: (VirtualRenderer) -> IO (GHCJS.JSBool)
foreign import javascript "$1.setHScrollBarAlwaysVisible($2)" setHScrollBarAlwaysVisible :: (VirtualRenderer) -> (GHCJS.JSBool) -> IO (())
foreign import javascript "$1.updateFrontMarkers()" updateFrontMarkers :: (VirtualRenderer) -> IO (())
foreign import javascript "$1.updateBackMarkers()" updateBackMarkers :: (VirtualRenderer) -> IO (())
foreign import javascript "$1.addGutterDecoration()" addGutterDecoration :: (VirtualRenderer) -> IO (())
foreign import javascript "$1.removeGutterDecoration()" removeGutterDecoration :: (VirtualRenderer) -> IO (())
foreign import javascript "$1.updateBreakpoints()" updateBreakpoints :: (VirtualRenderer) -> IO (())
foreign import javascript "$1.setAnnotations($2)" setAnnotations :: (VirtualRenderer) -> (GHCJS.JSArray (GHCJS.JSRef obj0)) -> IO (())
foreign import javascript "$1.updateCursor()" updateCursor :: (VirtualRenderer) -> IO (())
foreign import javascript "$1.hideCursor()" hideCursor :: (VirtualRenderer) -> IO (())
foreign import javascript "$1.showCursor()" showCursor :: (VirtualRenderer) -> IO (())
foreign import javascript "$1.scrollCursorIntoView()" scrollCursorIntoView :: (VirtualRenderer) -> IO (())
foreign import javascript "$1.getScrollTop()" getScrollTop :: (VirtualRenderer) -> IO (GHCJS.JSNumber)
foreign import javascript "$1.getScrollLeft()" getScrollLeft :: (VirtualRenderer) -> IO (GHCJS.JSNumber)
foreign import javascript "$1.getScrollTopRow()" getScrollTopRow :: (VirtualRenderer) -> IO (GHCJS.JSNumber)
foreign import javascript "$1.getScrollBottomRow()" getScrollBottomRow :: (VirtualRenderer) -> IO (GHCJS.JSNumber)
foreign import javascript "$1.scrollToRow($2)" scrollToRow :: (VirtualRenderer) -> (GHCJS.JSNumber) -> IO (())
foreign import javascript "$1.scrollToLine($2,$3,$4,$5)" scrollToLine :: (VirtualRenderer) -> (GHCJS.JSNumber) -> (GHCJS.JSBool) -> (GHCJS.JSBool) -> (Function) -> IO (())
foreign import javascript "$1.scrollToY($2)" scrollToY :: (VirtualRenderer) -> (GHCJS.JSNumber) -> IO (GHCJS.JSNumber)
foreign import javascript "$1.scrollToX($2)" scrollToX :: (VirtualRenderer) -> (GHCJS.JSNumber) -> IO (GHCJS.JSNumber)
foreign import javascript "$1.scrollBy($2,$3)" scrollBy :: (VirtualRenderer) -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (())
foreign import javascript "$1.isScrollableBy($2,$3)" isScrollableBy :: (VirtualRenderer) -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (GHCJS.JSBool)
foreign import javascript "$1.textToScreenCoordinates($2,$3)" textToScreenCoordinates :: (VirtualRenderer) -> (GHCJS.JSNumber) -> (GHCJS.JSNumber) -> IO (GHCJS.JSRef obj0)
foreign import javascript "$1.visualizeFocus()" visualizeFocus :: (VirtualRenderer) -> IO (())
foreign import javascript "$1.visualizeBlur()" visualizeBlur :: (VirtualRenderer) -> IO (())
foreign import javascript "$1.showComposition($2)" showComposition :: (VirtualRenderer) -> (GHCJS.JSNumber) -> IO (())
foreign import javascript "$1.setCompositionText($2)" setCompositionText :: (VirtualRenderer) -> (GHCJS.JSString) -> IO (())
foreign import javascript "$1.hideComposition()" hideComposition :: (VirtualRenderer) -> IO (())
foreign import javascript "$1.setTheme($2)" setTheme :: (VirtualRenderer) -> (GHCJS.JSString) -> IO (())
foreign import javascript "$1.getTheme()" getTheme :: (VirtualRenderer) -> IO (GHCJS.JSString)
foreign import javascript "$1.setStyle($2)" setStyle :: (VirtualRenderer) -> (GHCJS.JSString) -> IO (())
foreign import javascript "$1.unsetStyle($2)" unsetStyle :: (VirtualRenderer) -> (GHCJS.JSString) -> IO (())
foreign import javascript "$1.destroy()" destroy :: (VirtualRenderer) -> IO (())
