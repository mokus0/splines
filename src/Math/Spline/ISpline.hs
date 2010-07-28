{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances, FlexibleContexts, UndecidableInstances,
        ParallelListComp,
        StandaloneDeriving
  #-}
module Math.Spline.ISpline
    ( ISpline, iSpline
    ) where

import Math.Spline.BSpline
import Math.Spline.Class
import Math.Spline.Knots

import Data.Maybe (fromMaybe)
import Data.VectorSpace

-- |The I-Spline basis functions are the integrals of the M-splines, or
-- alternatively the integrals of the B-splines normalized to the range
-- [0,1].
data ISpline v = ISpline
    { iSplineDegree        :: !Int
    , iSplineKnotVector    :: Knots (Scalar v)
    , iSplineControlPoints :: [v]
    }

deriving instance (Eq   (Scalar v), Eq   v) => Eq   (ISpline v)
deriving instance (Ord  (Scalar v), Ord  v) => Ord  (ISpline v)
instance (Show (Scalar v), Show v) => Show (ISpline v) where
    showsPrec p (ISpline _ kts cps) = showParen (p>10) 
        ( showString "iSpline "
        . showsPrec 11 kts
        . showChar ' '
        . showsPrec 11 cps
        )


-- |@iSpline kts cps@ creates an I-spline with the given knot vector and control 
-- points.  The degree is automatically inferred as the difference between the 
-- number of spans in the knot vector (@numKnots kts - 1@) and the number of 
-- control points (@length cps@).
iSpline :: Knots (Scalar a) -> [a] -> ISpline a
iSpline kts cps 
    | n > m     = error "iSpline: too few knots"
    | otherwise = ISpline (m - n) kts cps
    where
        n = length cps
        m = numKnots kts - 1

instance (VectorSpace v, Fractional (Scalar v), Ord (Scalar v)) => Spline ISpline v where
    splineDegree = (1 +) . iSplineDegree
    knotVector spline = knotsFromList (head ts : ts ++ [last ts])
        where ts = knots (iSplineKnotVector spline)
    toBSpline spline = bSpline (knotVector spline) (scanl (^+^) zeroV cs)
        where cs = iSplineControlPoints spline

instance Spline ISpline v => ControlPoints ISpline v where
    mapControlPoints f (ISpline n ks cs) = ISpline n ks (map f cs)
    controlPoints      (ISpline _  _ cs) = cs
