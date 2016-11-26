module Main where

import Prelude
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Math (abs)
import Data.Array ((..))
import Data.Int (toNumber, round)
import Data.Maybe (Maybe(Just, Nothing))
import Graphics.Canvas (getCanvasElementById, CANVAS, getContext2D, setFillStyle, fillRect, Context2D, scale, transform, Transform, translate)
import DOM (DOM)
import Wireframe (getWireframeDataById)

newtype Point2d = Point2d { x :: Int, y :: Int }

instance showPoint2d :: Show Point2d where
  show (Point2d p) = "(" <> (show p.x) <> "," <> (show p.y) <> ")"

reflectInDiagonal :: Point2d -> Point2d
reflectInDiagonal (Point2d p) = Point2d { x: p.y, y: p.x }

fillPoint :: forall e. Context2D -> Int -> Int -> Eff ( canvas :: CANVAS | e ) Context2D
fillPoint ctx x y = fillRect ctx { x: toNumber x, y: toNumber y, w: 1.0, h: 1.0 }

genLineNonSteep :: Int -> Int -> Int -> Int -> Array Point2d
genLineNonSteep x0 y0 x1 y1 = map f (x0..x1)
  where f x = let t = toNumber (x - x0) / toNumber (x1 - x0)
                  y = toNumber y0 * (1.0 - t) + toNumber y1 * t
              in Point2d { x: x, y: round y }

generateLine :: Point2d -> Point2d -> Array Point2d
generateLine (Point2d p0) (Point2d p1) = points
  where
    horizProj = abs <<< toNumber $ p1.x - p0.x
    vertProj = abs <<< toNumber $ p1.y - p0.y
    points = if horizProj >= vertProj
             then genLineNonSteep p0.x p0.y p1.x p1.y
             else map reflectInDiagonal $ genLineNonSteep p0.y p0.x p1.y p1.x

drawLine :: forall e. Context2D -> String -> Int -> Int -> Int -> Int -> Eff ( canvas :: CANVAS | e ) Unit
drawLine ctx color x0 y0 x1 y1 = do
  setFillStyle color ctx
  let line = generateLine (Point2d { x: x0, y: y0 }) (Point2d { x: x1, y: y1 })
  foreachE line \(Point2d p) -> do
    fillPoint ctx p.x p.y
    pure unit

clear :: forall e. Context2D -> Eff ( canvas :: CANVAS | e ) Context2D
clear ctx = do
  setFillStyle "rgb(0, 0, 0)" ctx
  fillRect ctx { x: 0.0, y: 0.0, w: 800.0, h: 600.0 }

transformIdentity :: Transform
transformIdentity = { m11: 1.0, m21: 0.0, m31: 0.0,
                      m12: 0.0, m22: 1.0, m32: 0.0 }

main :: forall e. Eff (console :: CONSOLE, canvas :: CANVAS, dom :: DOM | e) Unit
main = do
  canvasElement <- getCanvasElementById "canvas"
  headData <- getWireframeDataById "head-data"
  case headData of
    Nothing -> log "no wireframe data"
    Just wireframeGetter -> do
      wireframe <- wireframeGetter
      log wireframe
      case canvasElement of
        Just element -> do
                         ctx <- getContext2D element
                         translate { translateX: 0.0, translateY: 600.0 } ctx
                         scale { scaleX: 2.0, scaleY: -2.0 } ctx
                         clear ctx
                         drawLine ctx "#0000ff" 13 220 80 240
                         drawLine ctx "#00ff00" 113 220 120 280
                         drawLine ctx "#00ffff" 280 220 250 280
                         drawLine ctx "#ff0000" 380 220 313 240
                         drawLine ctx "#ff00ff" 80 140 13 120
                         drawLine ctx "#ffff00" 120 180 113 120
                         drawLine ctx "#ffffff" 250 180 280 120
                         drawLine ctx "#cccccc" 313 140 380 120
                         drawLine ctx "#0000ff" 13 20 13 70
                         drawLine ctx "#0000ff" 23 70 23 20
                         drawLine ctx "#00ff00" 33 50 83 50
                         drawLine ctx "#00ff00" 83 30 33 30
                         transform transformIdentity ctx
                         pure unit
        Nothing -> log "sorry"
