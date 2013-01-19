{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances, FlexibleContexts, UndecidableInstances,
        ParallelListComp,
        StandaloneDeriving
  #-}
module Math.Spline.ISpline
    ( ISpline, iSpline, toISpline
    , evalSpline
    ) where

import Math.Spline.BSpline
import Math.Spline.Class
import Math.Spline.Knots
import qualified Data.Vector as V
import Data.VectorSpace

-- |The I-Spline basis functions are the integrals of the M-splines, or
-- alternatively the integrals of the B-splines normalized to the range
-- [0,1].  Every I-spline basis function increases monotonically from 0 to 1,
-- thus it is useful as a basis for monotone functions.  An I-Spline curve
-- is monotone if and only if every non-zero control point has the same sign.
data ISpline v = ISpline
    { iSplineDegree        :: !Int
    , iSplineKnotVector    :: Knots (Scalar v)
    , iSplineControlPoints :: !(V.Vector v)
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
iSpline :: Knots (Scalar a) -> V.Vector a -> ISpline a
iSpline kts cps 
    | n > m     = error "iSpline: too few knots"
    | otherwise = ISpline (m - n) kts cps
    where
        n = V.length cps
        m = numKnots kts - 1

instance (VectorSpace v, Fractional (Scalar v), Ord (Scalar v)) => Spline ISpline v where
    splineDegree = (1 +) . iSplineDegree
    knotVector spline = mkKnots (head ts : ts ++ [last ts])
        where ts = knots (iSplineKnotVector spline)
    toBSpline spline = bSpline (knotVector spline) (V.scanl (^+^) zeroV cs)
        where cs = iSplineControlPoints spline

instance Spline ISpline v => ControlPoints ISpline v where
    controlPoints      (ISpline _  _ cs) = cs

toISpline :: (Spline s v, Eq v) => s v -> ISpline v
toISpline = fromBSpline . toBSpline

fromBSpline :: (Eq v, VectorSpace v, Fractional (Scalar v), Ord (Scalar v))
    => BSpline V.Vector v -> ISpline v
fromBSpline spline
    | V.head ds == zeroV 
    && numKnots ks >= 2 = iSpline (mkKnots (init (tail ts))) (V.tail ds')
    | otherwise         = iSpline (mkKnots (init       ts )) ds'
    where
        ks = knotVector spline
        ts = knots ks
        ds = controlPoints spline
        
        ds' = V.zipWith (^-^) ds (V.cons zeroV ds)
