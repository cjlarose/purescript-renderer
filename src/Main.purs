module Main where

import Prelude
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array ((..))
import Data.Int (toNumber, round)
import Data.Maybe (Maybe(Just, Nothing))
import Graphics.Canvas (getCanvasElementById, CANVAS, getContext2D, setFillStyle, fillRect, Context2D, scale, transform, Transform, translate)

newtype Point2d = Point2d { x :: Int, y :: Int }

instance showPoint2d :: Show Point2d where
  show (Point2d p) = "(" <> (show p.x) <> "," <> (show p.y) <> ")"

fillPoint :: forall e. Context2D -> Int -> Int -> Eff ( canvas :: CANVAS | e ) Context2D
fillPoint ctx x y = fillRect ctx { x: toNumber x, y: toNumber y, w: 1.0, h: 1.0 }

generateLine :: Point2d -> Point2d -> Array Point2d
generateLine (Point2d p0) (Point2d p1) = map f (p0.x..p1.x)
  where
    f x = let t = toNumber (x - p0.x) / toNumber (p1.x - p0.x)
              y = toNumber p0.y * (1.0 - t) + toNumber p1.y * t
          in Point2d { x: x, y: round y }

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

main :: forall e. Eff (console :: CONSOLE, canvas :: CANVAS | e) Unit
main = do
  canvasElement <- getCanvasElementById "canvas"
  case canvasElement of
    Just element -> do
                     ctx <- getContext2D element
                     translate { translateX: 0.0, translateY: 600.0 } ctx
                     scale { scaleX: 2.0, scaleY: -2.0 } ctx
                     clear ctx
                     drawLine ctx "#ffffff" 13 20 80 40
                     drawLine ctx "#ff0000" 20 13 40 80
                     drawLine ctx "#ff0000" 80 40 13 20
                     transform transformIdentity ctx
                     pure unit
    Nothing -> log "sorry"
