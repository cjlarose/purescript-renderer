module Wireframe where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe)
import DOM (DOM)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode, elementToNode)
import DOM.Node.Node (textContent)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Types (htmlDocumentToDocument)
import Data.Nullable (toMaybe)

getWireframeDataById :: forall e0 e1. String -> Eff ( dom :: DOM | e0 ) (Maybe (Eff ( dom :: DOM | e1) String))
getWireframeDataById elId = do
  win <- window
  doc <- document win
  let nonElementParentNode = documentToNonElementParentNode <<< htmlDocumentToDocument $ doc
  nullableEl <- getElementById (ElementId elId) nonElementParentNode
  let maybeEl = toMaybe nullableEl
  pure $ textContent <<< elementToNode <$> maybeEl
