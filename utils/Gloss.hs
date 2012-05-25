-- utilities for visualizing splines and related calculations using Gloss
module Gloss where

import Data.VectorSpace
import Math.Spline
import Graphics.Gloss

tickHeight n
    | n `mod` 10 == 0   = 0.3
    | otherwise         = 0.15

xAxis x0 x1 = pictures
    ( line [(fromIntegral x0, 0), (fromIntegral x1, 0)]
    : [ line [ (fromIntegral x, -y) 
             , (fromIntegral x,  y) 
             ]
      | x <- [x0 .. x1]
      , let y = tickHeight x
      ])

yAxis y0 y1 = rotate 270 (xAxis y0 y1)

axes (x0,y0) (x1, y1) = pictures
    [ xAxis x0 x1
    , yAxis y0 y1
    ]

quickWin w h = display (InWindow "Gloss" (w,h) (20,20)) black . color white . scale s s
    where
        s = fromIntegral (max w h)

quickAnim w h = animate (InWindow "Gloss" (w,h) (20,20)) black . fmap (color white . scale s s)
    where
        s = fromIntegral (max w h)


circle' r (x,y) = translate x y (thickCircle 0 r)

bumpyLine r ps = pictures
    (line ps : map (circle' r) ps)


spline f = plot (evalSpline f) dx x0 x1
    where
        Just (x0, x1) = splineDomain f
        dx = (x1 - x0) / 1000

plot f dx x0 x1 = line
    [ f x
    | x <- [x0 , x0 + dx .. x1]
    ]