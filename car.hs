module Car where

import Graphics.UI.Threepenny

data Car = Car {pos :: Point, size :: Size, dir :: Double, speed :: Double, acc :: Double, maxSpeed :: Double, friction :: Double} deriving (Show)

type Size = (Double, Double)

newCar :: Double -> Double -> Car
newCar x y = Car (x, y) (40, 100) (- pi / 2) 0 0.4 10 0.15
