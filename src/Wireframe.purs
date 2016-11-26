module Wireframe where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe, fromJust)
import DOM (DOM)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode, elementToNode)
import DOM.Node.Node (textContent)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Types (htmlDocumentToDocument)
import Data.Nullable (toMaybe)
import Data.String (Pattern(..), split)
import Data.String.Regex (test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Array (filter, unsafeIndex, head)
import Data.Either (fromRight)
import Data.Int (floor)
import Global (readFloat, readInt)
import Partial.Unsafe (unsafePartial)

getWireframeDataById :: forall e0 e1. String -> Eff ( dom :: DOM | e0 ) (Maybe (Eff ( dom :: DOM | e1) String))
getWireframeDataById elId = do
  win <- window
  doc <- document win
  let nonElementParentNode = documentToNonElementParentNode <<< htmlDocumentToDocument $ doc
  nullableEl <- getElementById (ElementId elId) nonElementParentNode
  let maybeEl = toMaybe nullableEl
  pure $ textContent <<< elementToNode <$> maybeEl

newtype Face = Face { v0 :: Int, v1 :: Int, v2 :: Int }
newtype Vec3 = Vec3 { x :: Number, y :: Number, z :: Number }
newtype Model = Model { vertices :: Array Vec3, faces :: Array Face }

isBlank :: String -> Boolean
isBlank = test r
  where
    r = unsafePartial fromRight $ regex "^\\s*$" noFlags

parseWireframeData :: String -> Model
parseWireframeData str = Model { vertices: vertices, faces: faces }
  where
    splitBySpace :: String -> Array String
    splitBySpace = split (Pattern " ")

    lines :: Array (Array String)
    lines = map splitBySpace <<< filter (not <<< isBlank) $ split (Pattern "\n") str

    vertexData :: Array (Array String)
    vertexData = filter (\l -> (unsafePartial unsafeIndex l 0) == "v") lines

    vertices :: Array Vec3
    vertices = vertexData <#> (\arr -> Vec3 { x: readFloat $ unsafePartial unsafeIndex arr 1,
                                              y: readFloat $ unsafePartial unsafeIndex arr 2,
                                              z: readFloat $ unsafePartial unsafeIndex arr 3 })

    faceData :: Array (Array String)
    faceData = filter (\l -> (unsafePartial unsafeIndex l 0) == "f") lines

    vertexIndex :: String -> Int
    vertexIndex = floor <<< readInt 10 <<< unsafePartial fromJust <<< head <<< split (Pattern "/")

    faces :: Array Face
    faces = faceData <#> (\arr -> Face { v0: vertexIndex $ unsafePartial unsafeIndex arr 1,
                                         v1: vertexIndex $ unsafePartial unsafeIndex arr 2,
                                         v2: vertexIndex $ unsafePartial unsafeIndex arr 3 })
