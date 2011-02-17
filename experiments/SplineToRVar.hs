{-# LANGUAGE
        ParallelListComp,
        FlexibleContexts,
        TypeFamilies,
        MultiParamTypeClasses,
        FlexibleInstances,
        UndecidableInstances
  #-}
module SplineToRVar (splineDist, SplineDist) where

import Control.Monad
import Data.Maybe
import Data.Random
import Data.Random.Distribution.Categorical
import Data.VectorSpace
import Math.Root.Finder (eps)
import Math.Root.Finder.Brent
import Math.Spline
import Math.Spline.Knots
import Math.Spline.BSpline
import Math.Spline.BSpline.Reference

data SplineDist v = SplineDist !v !v (v -> v)

splineDist :: (Fractional v, Ord v, VectorSpace v, Scalar v ~ v) => BSpline v -> SplineDist v
splineDist s
    | not (isJust domain) = error "splineDist: spline has empty support"
    | otherwise = SplineDist lo hi int
        where
            f = toBSpline s
            ff = integrateBSpline f
            ffHi = evalSpline ff hi
            ffLo = evalSpline ff lo
            
            int x = (evalSpline ff x - ffLo) / (ffHi - ffLo)
            
            domain = splineDomain s
            Just (lo, hi) = domain
            
        

instance (Distribution StdUniform v, RealFloat v) => Distribution SplineDist v where
    rvar (SplineDist lo hi int) = do
        u <- stdUniform
        either (fail.show) return (brent (\x -> int x - u) lo hi (8 * eps))

instance (Distribution StdUniform v, RealFloat v) => CDF SplineDist v where
    cdf (SplineDist lo hi int) x
        | x <  lo   = 0
        | x >= hi   = 1
        | otherwise = clamp 0 1 . realToFrac . int $ x
        where
            clamp a b = max a . min b
