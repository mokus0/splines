{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

import Criterion.Main

import Data.List (find)
import qualified Data.Vector.Generic as V
import qualified Data.Vector as BV
import qualified Data.Vector.Unboxed as UV
import Math.Spline.BSpline
import Math.Spline.BSpline.Reference
import Math.Spline.Knots
import Math.Polynomial

import Debug.Trace
import Control.Monad

kts = mkKnots $ [0,0,0] ++ [0..10] ++ [11,11,11]
ctPts = map sin [0..12]

unboxedSpline :: BSpline UV.Vector Double
unboxedSpline = bSpline kts (V.fromList ctPts)

boxedSpline :: BSpline BV.Vector Double
boxedSpline = bSpline kts (V.fromList ctPts)


intervalPoly :: [(Double, Double, Poly Double)]
intervalPoly = map f $ zip3 dkts (tail dkts) (basisPolynomials kts)
  where
    dkts = distinctKnots kts
    f (begin, end, k) = (begin, end, ) . sumPolys $ zipWith scalePoly ctPts (k !! 3)

applyDeBoor :: V.Vector v Double => BSpline v Double -> Double -> Double
applyDeBoor s = evalBSpline s

{-# SPECIALIZE applyNaturalDeBoor :: BSpline BV.Vector Double -> Double -> Double #-}
{-# SPECIALIZE applyNaturalDeBoor :: BSpline UV.Vector Double -> Double -> Double #-}
applyNaturalDeBoor :: V.Vector v Double =>  BSpline v Double -> Double -> Double
applyNaturalDeBoor s = evalNaturalBSpline s

applyPoly :: Double -> Double
applyPoly x = maybe 0 (\(_,_,p) -> evalPoly p x) $ find (\(b,e,_) -> x >= b && x < e) intervalPoly

applyAndSum :: (Double -> Double) -> [Double] -> Double
applyAndSum f = sum . map f

main = defaultMain
       [ bgroup "Boxed"
         [ bench "deBoor 1000" $ whnf (applyAndSum (applyDeBoor boxedSpline)) [0,0.01..10]
         , bench "deBoor 10000" $ whnf (applyAndSum (applyDeBoor boxedSpline)) [0,0.001..10]
         , bench "natural 1000" $ whnf (applyAndSum (applyNaturalDeBoor boxedSpline)) [0,0.01..10]
         , bench "natural 10000" $ whnf (applyAndSum (applyNaturalDeBoor boxedSpline)) [0,0.001..10]
         ]
       , bgroup "Unboxed"
         [ bench "deBoor 1000" $ whnf (applyAndSum (applyDeBoor unboxedSpline)) [0,0.01..10]
         , bench "deBoor 10000" $ whnf (applyAndSum (applyDeBoor unboxedSpline)) [0,0.001..10]
         , bench "natural 1000" $ whnf (applyAndSum (applyNaturalDeBoor unboxedSpline)) [0,0.01..10]
         , bench "natural 10000" $ whnf (applyAndSum (applyNaturalDeBoor unboxedSpline)) [0,0.001..10]
         ]
       , bench "poly 1000" $ whnf (applyAndSum applyPoly) [0,0.01..10]
       , bench "poly 10000" $ whnf (applyAndSum applyPoly) [0,0.001..10]
       ]
