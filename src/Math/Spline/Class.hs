{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Math.Spline.Class where

import Control.Applicative
import Math.Spline.Knots
import qualified Math.Spline.BSpline.Internal as BSpline

import Data.VectorSpace

-- |A spline is a piecewise polynomial vector-valued function.  Minimum instance
-- definition is @toBSpline@.
class (VectorSpace v, Fractional (Scalar v), Ord (Scalar v)) => Spline s v where
    -- |Returns the domain of a spline.  In the case of B-splines, this is
    -- the domain on which a spline with this degree and knot vector has a 
    -- full basis set.  In other cases, it should be no larger than 
    -- @splineDomain . toBSpline@, but may be smaller.
    splineDomain :: s v -> Maybe (Scalar v, Scalar v)
    splineDomain = knotDomain <$> knotVector <*> splineDegree
    
    evalSpline :: s v -> Scalar v -> v
    evalSpline = evalSpline . toBSpline
    
    splineDegree :: s v -> Int
    splineDegree = splineDegree . toBSpline
    
    knotVector :: s v -> Knots (Scalar v)
    knotVector = knotVector . toBSpline
    
    toBSpline :: s v -> BSpline.BSpline v

class Spline s v => ControlPoints s v where
    controlPoints :: s v -> [v]

instance (VectorSpace v, Fractional (Scalar v), Ord (Scalar v)) => Spline BSpline.BSpline v where
    evalSpline spline = head . last . BSpline.deBoor spline
    splineDegree = BSpline.degree
    knotVector = BSpline.knotVector
    toBSpline = id

instance Spline BSpline.BSpline v => ControlPoints BSpline.BSpline v where
    controlPoints = BSpline.controlPoints