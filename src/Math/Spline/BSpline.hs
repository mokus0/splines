{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies, ParallelListComp #-}
module Math.Spline.BSpline
    ( BSpline
    , bSpline
    , evalBSpline
    , evalNaturalBSpline
    , insertKnot
    , splitBSpline
    , differentiateBSpline, integrateBSpline
    , deBoor
    , basisFuns
    , basisFuns'
    ) where

import Math.Spline.Knots
import Math.Spline.BSpline.Internal

import Data.Maybe (fromMaybe)
import Data.List
import Data.VectorSpace
import qualified Data.Vector.Generic as V
import Data.Vector.Generic ((!))

-- |@bSpline kts cps@ creates a B-spline with the given knot vector and control 
-- points.  The degree is automatically inferred as the difference between the 
-- number of spans in the knot vector (@numKnots kts - 1@) and the number of 
-- control points (@length cps@).
bSpline :: V.Vector v a => Knots (Scalar a) -> v a -> BSpline v a
bSpline kts cps = fromMaybe
    (error "bSpline: too few knots")
    (maybeSpline kts cps)

-- not exported; precondition: n > 0
maybeSpline :: V.Vector v a => Knots (Scalar a) -> v a -> Maybe (BSpline v a)
maybeSpline kts cps 
    | n > m     = Nothing
    | otherwise = Just (Spline (m - n) kts cps)
    where
        n = V.length cps
        m = numKnots kts - 1

differentiateBSpline
    :: (VectorSpace a, Fractional (Scalar a), Ord (Scalar a), V.Vector v a
       , V.Vector v (Scalar a)) => BSpline v a -> BSpline v a
differentiateBSpline spline
    | V.null ds = error "differentiateBSpline: no control points"
    | m  < 1    = spline
    | p == 0    = bSpline ks (V.replicate n zeroV)
    | otherwise = bSpline ks' ds'
    where
        n = V.length ds
        m = numKnots ks - 1
        
        ks' = mkKnots . init . tail $ ts
        ds' = V.zipWith (*^) (V.tail cs) (V.zipWith (^-^) (V.tail ds) ds)
        
        ks = knotVector spline; ts = knots ks
        ds = controlPoints spline
        
        p  = degree spline
        cs = V.fromList [ if t1 /= t0 then fromIntegral p / (t1 - t0) else 0 | (t0,t1) <- spans p ts]

integrateBSpline
  :: (VectorSpace a, Fractional (Scalar a), Ord (Scalar a), V.Vector v a
     , V.Vector v (Scalar a)) =>
     BSpline v a -> BSpline v a
integrateBSpline spline = bSpline (mkKnots ts') (V.scanl (^+^) zeroV ds')
    where
        ds' = V.zipWith (*^) cs (controlPoints spline)
        ts = knots (knotVector spline)
        ts' = head ts : ts ++ [last ts]
        p = degree spline + 1
        cs = V.fromList [(t1 - t0) / fromIntegral p | (t0,t1) <- spans p ts]

spans :: Int -> [a] -> [(a,a)]
spans n xs = zip xs (drop n xs)

-- |Split a B-spline at the specified point (which must be inside the spline's domain),
-- returning two disjoint splines, the sum of which is equal to the original.  The domain
-- of the first will be below the split point and the domain of the second will be above.
splitBSpline
  :: ( VectorSpace a, Ord (Scalar a), Fractional (Scalar a), V.Vector v a
     , V.Vector v (Scalar a)) =>
     BSpline v a -> Scalar a -> Maybe (BSpline v a, BSpline v a)
splitBSpline spline@(Spline p kv _) t 
    | inDomain  = Just (Spline p us0 ds0, Spline p us1 ds1)
    | otherwise = Nothing
    where
        inDomain = case knotDomain kv p of
            Nothing         -> False
            Just (t0, t1)   -> t >= t0 && t <= t1
        
        (lt, _, gt) = splitFind t kv
        dss = deBoor spline t
        
        us0 = setKnotMultiplicity t (p+1) lt
        ds0 = trimTo us0 (map V.head dss)
        
        us1 = setKnotMultiplicity t (p+1) gt
        ds1 = V.reverse (trimTo us1 (map V.last dss))
        
        trimTo kts = V.fromList . take (numKnots kts - p - 1)

basisFuns :: ( VectorSpace a, Ord (Scalar a), Fractional (Scalar a), V.Vector v a
     , V.Vector v (Scalar a)) =>
     BSpline v a -> Scalar a -> [Scalar a]
basisFuns (Spline p k _) u = basisFuns' p k u

-- Piegl & Tiller A2.2
-- As unintuitive as P&T's pseudo-c, but much less efficient
basisFuns' :: (Ord a, Fractional a) => Int -> Knots a -> a -> [a]
basisFuns' p ks u = case findSpan ks u of
  Nothing -> []
  Just i0 -> basis i0 p  where
    k = knotsVector ks
    left x  = u - k ! (i0+1-x)
    right x = k ! (i0+x) - u
    basis _ 0 = [1]
    -- calculate basis functions of degree j from those of degree j-1
    basis i j = outer $ foldl' (inner j) (0,0,[]) (basis i (j-1)) where
    outer (saved, _, n) = reverse $ (saved:n)
    inner j (saved, r, n) lower = (s, r', n') where
      s = left (j-r) * temp
      r' = r+1
      n' = (saved + right (r+1) * temp):n
      temp = lower / (right (r+1) + left (j-r))