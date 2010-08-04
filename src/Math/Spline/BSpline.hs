{-# LANGUAGE MultiParamTypeClasses, StandaloneDeriving, FlexibleContexts, UndecidableInstances, TypeFamilies, ParallelListComp #-}
module Math.Spline.BSpline
    ( BSpline
    , bSpline
    , evalBSpline
    , insertKnot
    , splitBSpline
    , derivBSpline, integrateBSpline
    ) where

import Math.Spline.Knots
import Math.Spline.BSpline.Internal

import Data.Maybe (fromMaybe)
import Data.VectorSpace

-- |@bSpline kts cps@ creates a B-spline with the given knot vector and control 
-- points.  The degree is automatically inferred as the difference between the 
-- number of spans in the knot vector (@numKnots kts - 1@) and the number of 
-- control points (@length cps@).
bSpline :: Knots (Scalar a) -> [a] -> BSpline a
bSpline   _  [] = error "bSpline: no control points"
bSpline kts cps = fromMaybe (error "bSpline: too few knots") (maybeSpline kts cps)

maybeSpline :: Knots (Scalar a) -> [a] -> Maybe (BSpline a)
maybeSpline kts cps 
    | n > m     = Nothing
    | otherwise = Just (Spline (m - n) kts cps)
    where
        n = length cps
        m = numKnots kts - 1

deriving instance (Eq   (Scalar v), Eq   v) => Eq   (BSpline v)
deriving instance (Ord  (Scalar v), Ord  v) => Ord  (BSpline v)
instance (Show (Scalar v), Show v) => Show (BSpline v) where
    showsPrec p (Spline _ kts cps) = showParen (p>10) 
        ( showString "bSpline "
        . showsPrec 11 kts
        . showChar ' '
        . showsPrec 11 cps
        )

derivBSpline
  :: (VectorSpace v, Fractional (Scalar v), Ord (Scalar v)) => BSpline v -> BSpline v
derivBSpline spline
    | numKnots ks  < 2  = spline
    | numKnots ks == 2  = bSpline ks [zeroV]
    | otherwise         = bSpline ks' ds'
    where
        ks' = mkKnots . init . tail $ ts
        ds' = zipWith (*^) (tail cs) (zipWith (^-^) (tail ds) ds)
        
        ks = knotVector spline; ts = knots ks
        ds = controlPoints spline
        
        p  = degree spline
        cs = [fromIntegral p / (t1 - t0) | (t0,t1) <- spans p ts]

integrateBSpline
  :: (VectorSpace v, Fractional (Scalar v), Ord (Scalar v)) => BSpline v -> BSpline v
integrateBSpline spline = bSpline (mkKnots ts') (scanl (^+^) zeroV ds')
    where
        ds' = zipWith (*^) cs (controlPoints spline)
        ts = knots (knotVector spline)
        ts' = head ts : ts ++ [last ts]
        p = degree spline + 1
        cs = [(t1 - t0) / fromIntegral p | (t0,t1) <- spans p ts]

spans n xs = zip xs (drop n xs)

splitBSpline
  :: (VectorSpace v, Ord (Scalar v), Fractional (Scalar v)) =>
     BSpline v -> Scalar v -> Maybe (BSpline v, BSpline v)
splitBSpline spline@(Spline p kv ds) t 
    | inDomain  = Just (Spline p (mkKnots us0) ds0, Spline p (mkKnots us1) ds1)
    | otherwise = Nothing
    where
        inDomain = case knotDomain kv p of
            Nothing         -> False
            Just (t0, t1)   -> t >= t0 || t <= t1
        
        us = knots kv
        dss = deBoor spline t
        
        us0 = takeWhile (<t) us ++ replicate (p+1) t
        ds0 = trimTo (drop (p+1) us0) (map head dss)
        
        us1 = replicate (p+1) t ++ dropWhile (<=t) us
        ds1 = reverse (trimTo (drop (p+1) us1) (map last dss))

        trimTo list  xs = zipWith const xs list
