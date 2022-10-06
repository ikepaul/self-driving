module Sensor where

import Car
import Graphics.UI.Threepenny hiding (map)
import Utils

data Sensor = Sensor {sensorCount :: Int, sensorLength :: Double, sensorSpread :: Double, sensorCar :: Car} deriving (Show)

data Ray = Ray {rayAngle :: Double, rayStart :: Point, rayEnd :: Point} deriving (Show)

initSensor :: Car -> Sensor
initSensor c = Sensor 3 100 (pi / 4) c

getRays :: Sensor -> [Ray]
getRays s = map (getRay s) [0 .. (n -1)]
  where
    (Sensor n _ _ _) = s

getRay :: Sensor -> Int -> Ray
getRay (Sensor n l s c) i = Ray rAngle startPos endPos
  where
    endPos = (cx - sin rAngle * l, cy - cos rAngle * l)
    startPos = pos c
    (cx, cy) = pos c
    rAngle = lerp s (- s) (if n == 1 then 0.5 else fromIntegral i / fromIntegral (n - 1)) + (dir c + pi / 2)
