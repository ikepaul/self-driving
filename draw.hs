module Draw where

import Car
import qualified Control.Monad
import Data.Fixed (mod')
import Graphics.UI.Threepenny
import Road
import Sensor
import Utils (lerp)

draw :: Element -> UI ()
draw canvas = do
  pure canvas # set fillStyle (solidColor (RGB 150 150 150))
  fillRect (10, 20) 300 100 canvas

drawCanvas :: Element -> UI ()
drawCanvas canvas = do
  pure canvas # set fillStyle (solidColor (RGB 190 190 190))
  fillRect (0, 0) 300 600 canvas

drawCircle :: Element -> Point -> UI ()
drawCircle canvas (x, y) = do
  beginPath canvas
  arc (x, y) 20 0 (2 * pi) canvas
  fill canvas

drawCar :: Element -> Car -> UI ()
drawCar canvas (Car (x, y) (w, h) d _ _ _ _) = do
  pure canvas # set lineWidth w
  pure canvas # set fillStyle (solidColor (RGB 0 0 0))
  pure canvas # set strokeStyle "rgb(0,0,0)"

  beginPath canvas
  moveTo (x + cos d * (h / 2), y + sin d * (h / 2)) canvas
  lineTo (x - cos d * (h / 2), y - sin d * (h / 2)) canvas
  stroke canvas

drawFixedCar :: Element -> Car -> Double -> UI ()
drawFixedCar canvas (Car (x, _) (w, h) d _ _ _ _) y = do
  pure canvas # set lineWidth w
  pure canvas # set fillStyle (solidColor (RGB 0 0 0))
  pure canvas # set strokeStyle "rgb(0,0,0)"

  beginPath canvas
  moveTo (x + cos d * (h / 2), y + sin d * (h / 2)) canvas
  lineTo (x - cos d * (h / 2), y - sin d * (h / 2)) canvas
  stroke canvas

drawRoad :: Element -> Road -> Double -> UI ()
drawRoad canvas r y = do
  let dashStep = 30
  let offset = - y `mod'` (dashStep * 2)
  let (Road rx rw rl bs) = r
  drawCanvas canvas
  pure canvas # set lineWidth 5
  pure canvas # set strokeStyle "rgb(255,255,255)"

  mapM_ (uncurry (drawBorder canvas)) bs
  Control.Monad.when (rl > 1) $ mapM_ (drawDashedLines canvas dashStep offset . lerp (roadLeft r) (roadRight r) . (/ fromIntegral rl) . fromIntegral) [1 .. rl]

drawBorder :: Element -> Point -> Point -> UI ()
drawBorder canvas (x1, y1) (x2, y2) = do
  pure canvas # set lineWidth 5
  pure canvas # set strokeStyle "rgb(255,255,255)"

  beginPath canvas
  moveTo (x1, y1) canvas
  lineTo (x2, y2) canvas
  stroke canvas

drawLine :: Element -> Double -> UI ()
drawLine canvas x = drawBorder canvas (x, roadTop) (x, roadBottom)

drawDashedLines :: Element -> Double -> Double -> Double -> UI ()
drawDashedLines canvas step offset x = do
  mapM_ (drawDashedLine canvas x step . (offset +) . (2 * step *) . flip (-) 5) [1 .. 30]

drawDashedLine :: Element -> Double -> Double -> Double -> UI ()
drawDashedLine canvas x step y = do
  pure canvas # set lineWidth 5
  pure canvas # set strokeStyle "rgb(255,255,255)"

  beginPath canvas
  moveTo (x, y) canvas
  lineTo (x, y + step) canvas
  stroke canvas

drawFixedSensor :: Element -> Sensor -> Double -> UI ()
drawFixedSensor canvas sensor offset = do
  let rays = getRays sensor
  mapM_ (drawFixedRay canvas offset) rays

drawFixedRay :: Element -> Double -> Ray -> UI ()
drawFixedRay canvas offset (Ray a (sx, sy) (ex, ey)) = do
  pure canvas # set lineWidth 3
  pure canvas # set strokeStyle "yellow"
  moveTo (sx, offset) canvas
  lineTo (ex, offset - (sy - ey)) canvas
  stroke canvas

drawSensor :: Element -> Sensor -> UI ()
drawSensor canvas sensor = do
  let rays = getRays sensor
  mapM_ (drawRay canvas) rays

drawRay :: Element -> Ray -> UI ()
drawRay canvas (Ray a s e) = do
  pure canvas # set lineWidth 3
  pure canvas # set strokeStyle "yellow"
  moveTo s canvas
  lineTo e canvas
  stroke canvas
