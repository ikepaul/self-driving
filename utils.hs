module Utils where

import Graphics.UI.Threepenny

getIntersection :: Point -> Point -> Point -> Point -> Maybe Point
getIntersection (x1, y1) (x2, y2) (x3, y3) (x4, y4)
  | bottom /= 0 && t >= 0 && t <= 1 && u >= 0 && u <= 1 = Just (x5, y5)
  | otherwise = Nothing
  where
    x5 = lerp x1 x2 t
    y5 = lerp y1 y2 t
    t = tTop / bottom
    u = uTop / bottom
    tTop = (x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)
    uTop = (y3 - y1) * (x1 - x2) - (x3 - x1) * (y1 - y2)
    bottom = (y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1)

lerp :: Double -> Double -> Double -> Double
lerp a b t = a + (b - a) * t

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2