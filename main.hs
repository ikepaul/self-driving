{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Action
import Car
import qualified Control.Monad
import Data.IORef
import Data.Maybe
import Draw
import Graphics.UI.Threepenny
import Road
import Sensor

main :: IO ()
main = startGUI defaultConfig initGame

initGame :: Window -> UI ()
initGame window = do
  pure window # set title "testing"

  canvas <- canvas # set height 600 # set width 300

  body <- getBody window
  pure body
    #+ [column [element canvas]]
  pure body # set style [("background-color", "gray")]
  let startCar = newCar (laneCenter newRoad 1) 450
  carControllerRef <- liftIO $ newIORef (newController :: Controller)
  roadRef <- liftIO $ newIORef (newRoad :: Road)
  carRef <- liftIO $ newIORef (startCar :: Car)
  sensorRef <- liftIO $ newIORef (initSensor startCar :: Sensor)

  on keydown body $ \keyCode -> do
    let maybeEventAction = readAction keyCode
    Control.Monad.when (isJust maybeEventAction) $ do
      let eventAction = fromMaybe CarForward maybeEventAction
      carController <- liftIO $ modifyIORef carControllerRef (toggleController eventAction True)
      return ()

  on keyup body $ \keyCode -> do
    let maybeEventAction = readAction keyCode
    Control.Monad.when (isJust maybeEventAction) $ do
      let eventAction = fromMaybe CarForward maybeEventAction
      carController <- liftIO $ modifyIORef carControllerRef (toggleController eventAction False)
      return ()

  timer1 <- set interval 15 timer
  start timer1
  on tick timer1 $ \() -> gameLoop window canvas carRef carControllerRef roadRef sensorRef

  --gameLoop window canvas

  return ()

gameLoop :: Window -> Element -> IORef Car -> IORef Controller -> IORef Road -> IORef Sensor -> UI ()
gameLoop window canvas carRef carControllerRef roadRef sensorRef = do
  (Car (x, y) (w, h) d v a mv f) <- liftIO $ readIORef carRef
  (Controller cf cr cb cl) <- liftIO $ readIORef carControllerRef
  road <- liftIO $ readIORef roadRef
  (Sensor n l s c) <- liftIO $ readIORef sensorRef

  let dv = fromIntegral (fromEnum cf) * a - fromIntegral (fromEnum cb) * a
  let newV = calcV v dv mv f
  let newD = calcD newV d cr cl
  let dx = cos newD * newV
  let dy = sin newD * newV
  let myCar = Car (x + dx, y + dy) (w, h) newD newV a mv f
  let mySensor = Sensor n l s myCar

  pure canvas # set fillStyle (solidColor (RGB 100 100 100))
  drawRoad canvas road (y + dy)
  drawFixedSensor canvas mySensor 450
  drawFixedCar canvas myCar 450

  liftIO $ writeIORef carRef myCar

calcV v dv mv f
  | (v + dv) == 0 = 0
  | abs (v + dv) < f = 0
  | (v + dv) > 0 =
    if (v + dv - f) > mv
      then mv
      else v + dv - f
  | (v + dv + f) < - mv = - mv
  | otherwise = v + dv + f

calcD :: Double -> Double -> Bool -> Bool -> Double
calcD v d cr cl
  | v == 0 = d
  | v > 0 = d + fromIntegral (fromEnum cr) * 0.05 - fromIntegral (fromEnum cl) * 0.05
  | v < 0 = d - fromIntegral (fromEnum cr) * 0.05 + fromIntegral (fromEnum cl) * 0.05