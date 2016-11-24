module Main where

import Prelude
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Int (toNumber, floor)
import Data.Maybe (Maybe(Just, Nothing))
import Graphics.Canvas (getCanvasElementById, CANVAS, getContext2D, setFillStyle, fillRect, Context2D, scale, transform, Transform, translate)

fillPoint :: forall e. Context2D -> Int -> Int -> Eff ( canvas :: CANVAS | e ) Context2D
fillPoint ctx x y = fillRect ctx { x: toNumber x, y: toNumber y, w: 1.0, h: 1.0 }

drawLine :: forall e. Context2D -> String -> Int -> Int -> Int -> Int -> Eff ( canvas :: CANVAS | e ) Unit
drawLine ctx color x0 y0 x1 y1 = do
  setFillStyle color ctx
  forE 0 100 \i -> do
    let t = (toNumber i) / 100.0
    let x = (toNumber x0) * (1.0 - t) + (toNumber x1) * t
    let y = (toNumber y0) * (1.0 - t) + (toNumber y1) * t
    fillPoint ctx (floor x) (floor y)
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
