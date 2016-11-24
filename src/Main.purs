module Main where

import Prelude
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just, Nothing))
import Graphics.Canvas (getCanvasElementById, CANVAS, getContext2D, setFillStyle, fillRect, Context2D)

fillPoint :: forall e. Context2D -> Number -> Number -> Eff ( canvas :: CANVAS | e ) Context2D
fillPoint ctx x y = fillRect ctx { x: x, y: y, w: 1.0, h: 1.0 }

drawLine :: forall e. Context2D -> String -> Number -> Number -> Number -> Number -> Eff ( canvas :: CANVAS | e ) Unit
drawLine ctx color x0 y0 x1 y1 = do
  setFillStyle color ctx
  forE 0 100 \i -> do
    let t = (toNumber i) / 100.0
    let x = x0 * (1.0 - t) + x1 * t
    let y = y0 * (1.0 - t) + y1 * t
    fillPoint ctx x y
    pure unit

clear :: forall e. Context2D -> Eff ( canvas :: CANVAS | e ) Context2D
clear ctx = do
  setFillStyle "rgb(0, 0, 0)" ctx
  fillRect ctx { x: 0.0, y: 0.0, w: 800.0, h: 800.0 }

main :: forall e. Eff (console :: CONSOLE, canvas :: CANVAS | e) Unit
main = do
  log "Hello sailor!"
  canvasElement <- getCanvasElementById "canvas"
  case canvasElement of
    Just element -> do
                     ctx <- getContext2D element
                     clear ctx
                     drawLine ctx "#ff0000" 0.0 0.0 100.0 100.0
                     log "hello"
    Nothing -> log "sorry"
