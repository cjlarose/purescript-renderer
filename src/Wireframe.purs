module Wireframe where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(Just, Nothing))
import DOM (DOM)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode, elementToNode)
import DOM.Node.Node (textContent)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Types (htmlDocumentToDocument)
import Data.Nullable (toMaybe)

getWireframeDataById :: forall e. String -> Eff (dom :: DOM | e) String
getWireframeDataById elId = do
  win <- window
  doc <- document win
  let nonElementParentNode = documentToNonElementParentNode <<< htmlDocumentToDocument $ doc
  nullableEl <- getElementById (ElementId elId) nonElementParentNode
  let maybeEl = toMaybe nullableEl
  case maybeEl of
    Just element -> do
      textContent <<< elementToNode $ element
    Nothing -> do
      pure ""
