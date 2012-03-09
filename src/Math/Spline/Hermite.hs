{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Math.Spline.Hermite
    ( CSpline, cSpline
    , evalSpline
    ) where

import Data.List
import Data.Ord
import Math.Polynomial hiding (x)
import Math.Spline.BSpline
import Math.Spline.Class
import Math.Spline.Knots
import qualified Data.Vector.Safe as V
import Data.VectorSpace

-- | Cubic Hermite splines.  These are cubic splines defined by a 
-- sequence of control points and derivatives at those points.
data CSpline a = CSpline [(Scalar a,a,a)]

-- | Cubic splines specified by a list of control points, 
-- where each control point is given by a triple of parameter value, 
-- position of the spline at that parameter value,
-- and derivative of the spline at that parameter value.
cSpline :: Ord (Scalar a) => [(Scalar a,a,a)] -> CSpline a
cSpline = CSpline . sortBy (comparing fst3)
    where fst3 (a,_,_) = a

h00, h10, h01, h11 :: (Num a, Eq a) => Poly a
h00 = poly BE [ 2,-3, 0, 1]
h10 = poly BE [ 1,-2, 1, 0]
h01 = poly BE [-2, 3, 0, 0]
h11 = poly BE [ 1,-1, 0, 0]

evalHermite
  :: (Eq (Scalar v), Num (Scalar v), VectorSpace v) =>
     v -> v -> v -> v -> Scalar v -> v
evalHermite y0 m0 y1 m1 x = sumV
    [ evalPoly h x *^ p
    | p <- [ y0,  m0,  y1,  m1]
    | h <- [h00, h10, h01, h11]
    ]

evalCSpline
  :: (VectorSpace v, Ord (Scalar v), Fractional (Scalar v)) =>
     CSpline v -> Scalar v -> v
evalCSpline (CSpline cps) = loop cps
    where
        loop []         _ = zeroV
        loop [(_,y,_)]  _ = y
        loop ((x0,y0,m0):rest@((x1,y1,m1):_)) x
            | x <= x0   = y0
            | x <  x1   = let dx = x1 - x0
                           in evalHermite y0 (m0 ^* dx) y1 (m1 ^* dx) ((x - x0) / dx)
            | otherwise = loop rest x

instance (VectorSpace a, Fractional (Scalar a), Ord (Scalar a)) => Spline CSpline a where
    splineDegree _ = 3
    
    splineDomain (CSpline  []) = Nothing
    splineDomain (CSpline cps) = Just (head xs, last xs) where (xs, _, _) = unzip3 cps
    
    evalSpline = evalCSpline
    
    -- TODO: check.  Also work out a more compact translation, taking advantage of the
    -- known continuity on the interior knots.  It should be possible to work out an 
    -- equivalent b-spline with only 'n+4' knots.  If that translation isn't ill-
    -- conditioned, it might be a good thing to implement.
    toBSpline (CSpline cSpl) = bSpline kts (V.fromList cps)
        where 
            kts = fromList [(x,4) | x <- xs]
            
            (xs, _, _) = unzip3 cSpl
            cps = concat 
                [ [ y0
                  , y0 ^+^ dy0 ^* dx3
                  , y1 ^-^ dy1 ^* dx3
                  , y1
                  ]
                | ((x0, y0, dy0), (x1, y1, dy1)) <- spans 1 cSpl
                , let dx3 = (x1 - x0) / 3
                ]

spans :: Int -> [a] -> [(a,a)]
spans n xs = zip xs (drop n xs)

