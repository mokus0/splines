module Main where

import Math.Spline
import Math.Spline.BSpline
import Math.Spline.BSpline.Reference
import Gloss
import Graphics.Gloss

import qualified Data.Vector.Unboxed as UV

kts = mkKnots $ [0,0,0,0,0,0,0,2,2.5,5,6,7,10,10,10,10,10,10,10]
ctPts = 
    [ (2, 1)
    , (2, 2)
    , (1, 2)
    , (-0.5, -0.5)
    , (-3.4, 1.2)
    , (-1.5, -0.3)
    , (-3, -1)
    , (-0.8, -2.4)
    , (-0.3, -2.5)
    , (1.2, -2.7)
    , (2, -0.5)
    , (2, 1)
    ]

f :: BSpline UV.Vector Point
f = bSpline kts (UV.fromList ctPts)

deBoorColors = concat $ iterate (map light) [red, orange, yellow, green, blue, violet]

mod1 x = snd (properFraction x)
fracMod p q = mod1 (p/q) * q

main = do
    putStrLn . unlines $ map unwords
        [ ["degree:", show (splineDegree f)]
        , ["domain:", show (splineDomain f)]
        ]
    quickAnim 1200 800 $ \x' ->
        let x = x' `fracMod` 13
         in   scale (1/8) (1/8) 
            . translate 0 0.33
            $ pictures
                [ axes (-10, -10) (10, 10)
                , color (greyN 0.2) $ spline f
                
                , color (dark red) $ plot (evalNaturalBSpline f) 0.1 0 x
                , color yellow $ plot (evalSpline f) 0.1 0 x
                -- , color (dark green) $ plot (evalReferenceBSpline f) 0.1 0 x
                
                , pictures $ zipWith color deBoorColors
                    [ bumpyLine 0.1 (UV.toList l)
                    | l <- deBoor f x
                    ]
                , color yellow $ pictures $ map (circle' 0.15) ctPts
                , color white $ circle' 0.15 (evalSpline f x)
                ]
