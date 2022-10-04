module Road where

import Graphics.UI.Threepenny

data Road = Road {roadX :: Double, roadWidth :: Double, laneCount :: Int, borders :: [(Point, Point)]}

newRoad :: Road
newRoad = Road 150 280 3 [((10, roadTop), (10, roadBottom)), ((290, roadTop), (290, roadBottom))]

roadLeft :: Road -> Double
roadLeft (Road x w _ _) = x - w / 2

roadRight :: Road -> Double
roadRight (Road x w _ _) = x + w / 2

roadTop :: Double
roadTop = -1000000

roadBottom :: Double
roadBottom = 1000000

laneCenter :: Road -> Int -> Double
laneCenter r i = roadLeft r + laneWidth / 2 + fromIntegral i * laneWidth
  where
    laneWidth = w / fromIntegral lc
    (Road x w lc bs) = r