{-# LANGUAGE ParallelListComp #-}
-- |Reference implementation of B-Splines; very inefficient but \"obviously\"
-- correct.
module Math.Spline.BSpline.Reference where

import Math.Spline.Knots
import qualified Math.Polynomial as Poly
import Data.VectorSpace

ind True  = 1
ind False = 0

spans :: Int -> [a] -> [(a,a)]
spans     = spansWith (,)
spanDiffs :: Num a => Int -> [a] -> [a]
spanDiffs = spansWith subtract
spansWith f n ts = zipWith f ts (drop n ts)

-- Implementation following the usual definition, which is not how it's 
-- usually actually implemented - uppermost knot is not in domain of basis
-- functions.
-- TODO: cleanup from generalization, test 'basisPolynomialsAt'
bases kts x = basesAt kts x (/) id x
basisPolynomialsAt kts x = basesAt kts x (^/) Poly.constPoly Poly.x
basesAt kts xLoc (/) lift xVal = bs
    where
        ts = knots kts
        
        b = [ ind (t_j <= xLoc && xLoc < t_jp1)
            | (t_j, t_jp1)     <- spans 1 ts
            ]
        
        bs = b : takeWhile (not . null)
            [ [ (if d0 == 0 then 0 else (xVal         - lift t_j) / d0) * b_nm1_j
              + (if d1 == 0 then 0 else (lift t_jpnp1 -     xVal) / d1) * b_nm1_jp1
              | (b_nm1_j, b_nm1_jp1)    <- spans 1 prevBasis
              | (d0, d1)                <- spans 1 (spanDiffs n ts)
              | (t_j, t_jpnp1)          <- spans (n+1) ts
              ]
            | prevBasis <- bs
            | n <- [1..]
            ]
