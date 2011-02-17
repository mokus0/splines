{-# LANGUAGE
        FlexibleContexts,
        TypeFamilies,
        MultiParamTypeClasses,
        FlexibleInstances,
        UndecidableInstances
  #-}
module SplineToRVar where

import Control.Monad
import Data.Maybe
import Data.Random
import Data.Random.Distribution.Categorical
import Data.VectorSpace
import Math.Spline
import Math.Spline.Knots
import Math.Spline.BSpline
import Math.Spline.BSpline.Reference (bases)

newtype SplineDist s v = SplineDist (s v)

instance (Spline s v, VectorSpace v, Scalar v ~ v, Fractional v, Ord v, Distribution StdUniform v) => Distribution (SplineDist s) v where
    rvar (SplineDist s) = splineToRVar s

instance (Spline s v, VectorSpace v, Scalar v ~ v, Real v, Fractional v, Distribution StdUniform v) => CDF (SplineDist s) v where
    cdf (SplineDist s) x
        | not (isJust domain) = error "cdf: spline has empty support"
        | x <  lo   = 0
        | x >= hi   = 1
        | otherwise = clamp 0 1 (realToFrac ((evalSpline ff x - ffLo) / (ffHi - ffLo)))
        where
            f = toBSpline s
            ff = integrateBSpline f
            ffHi = evalSpline ff hi
            ffLo = evalSpline ff lo
            
            domain = splineDomain s
            Just (lo, hi) = domain
            
            clamp a b = max a . min b
        
-- An overall better strategy would probably be to compute an I-Spline for the
-- integral and invert that function, then just use it to remap a stdUniform.

splineToRVar :: (Spline s v, Fractional v, Ord v, Distribution StdUniform v, Distribution StdUniform (Scalar v)) => s v -> RVar (Scalar v)
splineToRVar s 
    | isJust domain = if lo == hi then return lo else inDomain
    | otherwise     = error "splineToRVar: spline has empty support"
        -- TODO: figure out an effective way to determine a priori whether
        -- the spline is negative anywhere inside its domain
    where
        -- convert to M-Spline because basis functions are automatically normalized
        -- so that their integrals (over the domain) are 1.  That way, the control
        -- points are valid selection weights for the basis functions
        ms = toMSpline s
        cps = controlPoints ms
        nonNegCPS = map (max 0) cps

        kts = knotVector ms
        envelope = mSpline kts nonNegCPS
        
        sampleSpline
            | all (>= 0) cps    = sampleEnvelope
            | otherwise         = sampleWithRejection
        
        sampleEnvelope = do
            basisFuncIndex <- rvar (weightedCategorical (zip nonNegCPS [0..]))
            sampleBasisFunction kts (splineDegree ms) basisFuncIndex
        
        sampleWithRejection = do
            x <- sampleEnvelope
            
            let fx = evalSpline s        x
                gx = evalSpline envelope x
            
            when (fx <  0) (fail ("spline is negative at " ++ show x))
            
            if fx == gx || gx == 0 then return x -- gx == 0  can happen, but (theoretically) only if the envelope is uniformly zero
            else do
                u <- stdUniform
                if u <= fx/gx then return x
                    else sampleWithRejection
        
        domain = splineDomain s
        Just (lo,hi) = domain
        inDomain = do
            x <- sampleSpline
            if lo <= x && x <= hi
                then return x
                else inDomain
                

-- very inefficient: simple rejection sample with uniform envelope.  If we're gonna
-- do this, then it's probably better to just do rejection sampling on the whole spline.
sampleBasisFunction :: (Fractional s, Ord s, Distribution StdUniform s) => Knots s -> Int -> Int -> RVar s
sampleBasisFunction kts deg i = loop
    where
        Just (lo,hi) = knotDomain kts deg
        
        f x = bases kts x !! deg !! i
        
        loop = do
            x <- uniform' lo hi
            u <- stdUniform
            if u <= f x then return x else loop

-- another idea that is "almost right", but not quite
-- sampleBasisFunction kts deg i = extract (bases !! deg !! i)
--     where
--         b0 = spansWith (\a b -> (b-a, a, b, uniform' a b)) (knots kts)
--         bases = iterate (spansWith conv) b0
--         extract (_,_,_,x) = x

uniform' a b = fmap (lerp a b) stdUniform
    where lerp x y a = (1-a)*x + a*y

-- weighted convolution, propagating weight.  This is the "interpolation" step of the cox-de boor recursion formula.
-- This is not quite correct, actually. TODO: work out the right weights to use
-- conv :: (Ord a, Fractional a, Distribution StdUniform a) => (a, Int, RVar a) -> (a, Int, RVar a) -> (a,Int,RVar a)
conv (w1,a1,b1,v1) (w2,a2,b2,v2) = (w1 + w2, min a1 a2, max b1 b2, v)
    where
        v = join (rvar (weightedCategorical [ (w1, liftM2 max v1 v1)-- (uniform' a1 a2) v1)
                                            , (w2, liftM2 min v2 v2)-- (uniform' b1 b2) v2)
                                            ]))

spansWith f xs = zipWith f xs (tail xs)
