{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies, ParallelListComp #-}
module Math.Spline.BSpline
    ( BSpline
    , bSpline
    , evalBSpline
    , insertKnot
    , splitBSpline
    , differentiateBSpline, integrateBSpline
    ) where

import Math.Spline.Knots
import Math.Spline.BSpline.Internal

import Data.Maybe (fromMaybe)
import Data.VectorSpace
import qualified Data.Vector as V

-- |@bSpline kts cps@ creates a B-spline with the given knot vector and control 
-- points.  The degree is automatically inferred as the difference between the 
-- number of spans in the knot vector (@numKnots kts - 1@) and the number of 
-- control points (@length cps@).
bSpline :: Knots (Scalar a) -> V.Vector a -> BSpline a
bSpline kts cps
    | V.null cps    = error "bSpline: no control points"
    | otherwise     = fromMaybe
        (error "bSpline: too few knots")
        (maybeSpline kts cps)

maybeSpline :: Knots (Scalar a) -> V.Vector a -> Maybe (BSpline a)
maybeSpline kts cps 
    | n > m     = Nothing
    | otherwise = Just (Spline (m - n) kts cps)
    where
        n = V.length cps
        m = numKnots kts - 1

differentiateBSpline
  :: (VectorSpace v, Fractional (Scalar v), Ord (Scalar v)) => BSpline v -> BSpline v
differentiateBSpline spline
    | numKnots ks  < 2  = spline
    | numKnots ks == 2  = bSpline ks (V.singleton zeroV)
    | otherwise         = bSpline ks' ds'
    where
        ks' = mkKnots . init . tail $ ts
        ds' = V.zipWith (*^) (V.tail cs) (V.zipWith (^-^) (V.tail ds) ds)
        
        ks = knotVector spline; ts = knots ks
        ds = controlPoints spline
        
        p  = degree spline
        cs = V.fromList [fromIntegral p / (t1 - t0) | (t0,t1) <- spans p ts]

integrateBSpline
  :: (VectorSpace v, Fractional (Scalar v), Ord (Scalar v)) => BSpline v -> BSpline v
integrateBSpline spline = bSpline (mkKnots ts') (V.scanl (^+^) zeroV ds')
    where
        ds' = V.zipWith (*^) cs (controlPoints spline)
        ts = knots (knotVector spline)
        ts' = head ts : ts ++ [last ts]
        p = degree spline + 1
        cs = V.fromList [(t1 - t0) / fromIntegral p | (t0,t1) <- spans p ts]

spans :: Int -> [a] -> [(a,a)]
spans n xs = zip xs (drop n xs)

splitBSpline
  :: (VectorSpace v, Ord (Scalar v), Fractional (Scalar v)) =>
     BSpline v -> Scalar v -> Maybe (BSpline v, BSpline v)
splitBSpline spline@(Spline p kv _) t 
    | inDomain  = Just (Spline p (mkKnots us0) ds0, Spline p (mkKnots us1) ds1)
    | otherwise = Nothing
    where
        inDomain = case knotDomain kv p of
            Nothing         -> False
            Just (t0, t1)   -> t >= t0 || t <= t1
        
        us = knots kv
        dss = deBoor spline t
        
        us0 = takeWhile (<t) us ++ replicate (p+1) t
        ds0 = V.fromList (trimTo (drop (p+1) us0) (map V.head dss))
        
        us1 = replicate (p+1) t ++ dropWhile (<=t) us
        ds1 = V.reverse (V.fromList (trimTo (drop (p+1) us1) (map V.last dss)))

        trimTo list  xs = zipWith const xs list
