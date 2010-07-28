{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances, FlexibleContexts, UndecidableInstances,
        ParallelListComp,
        StandaloneDeriving
  #-}
module Math.Spline.MSpline
    ( MSpline, mSpline, toMSpline
    ) where

import Math.Spline.BSpline
import Math.Spline.Class
import Math.Spline.Knots

import Data.VectorSpace

-- |M-Splines are B-splines normalized so that the integral of each basis 
-- function over the spline domain is 1.
data MSpline v = MSpline
    { mSplineDegree        :: !Int
    , mSplineKnotVector    :: Knots (Scalar v)
    , mSplineControlPoints :: [v]
    }

deriving instance (Eq   (Scalar v), Eq   v) => Eq   (MSpline v)
deriving instance (Ord  (Scalar v), Ord  v) => Ord  (MSpline v)
instance (Show (Scalar v), Show v) => Show (MSpline v) where
    showsPrec p (MSpline _ kts cps) = showParen (p>10) 
        ( showString "mSpline "
        . showsPrec 11 kts
        . showChar ' '
        . showsPrec 11 cps
        )


-- |@mSpline kts cps@ creates a M-spline with the given knot vector and control 
-- points.  The degree is automatically inferred as the difference between the 
-- number of spans in the knot vector (@numKnots kts - 1@) and the number of 
-- control points (@length cps@).
mSpline :: Knots (Scalar a) -> [a] -> MSpline a
mSpline kts cps
    | n > m     = error "mSpline: too few knots"
    | otherwise = MSpline (m - n) kts cps
    where
        n = length cps
        m = numKnots kts - 1

spans n xs = zip xs (drop n xs)

instance (VectorSpace v, Fractional (Scalar v), Ord (Scalar v)) => Spline MSpline v where
    splineDegree = mSplineDegree
    knotVector   = mSplineKnotVector
    toBSpline (MSpline p ks cs) = bSpline ks cs'
        where
            n = p + 1; n' = fromIntegral n
            cs' = [ (n' / (t1 - t0)) *^ c 
                  | c <- cs
                  | (t0, t1) <- spans n (knots ks)
                  ]

instance Spline MSpline v => ControlPoints MSpline v where
    controlPoints = mSplineControlPoints

toMSpline :: Spline s v => s v -> MSpline v
toMSpline = fromBSpline . toBSpline

fromBSpline spline = mSpline ks cs
    where
        n = splineDegree spline + 1; n' = fromIntegral n
        ks = knotVector spline
        cs =  [ ((t1 - t0) / n') *^ c
              | c <- controlPoints spline
              | (t0, t1) <- spans n (knots ks)
              ]
