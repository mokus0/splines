{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances, IncoherentInstances #-}
module Math.Spline.Class where

import Control.Applicative
import Math.Spline.Knots
import qualified Math.Spline.BSpline.Internal as BSpline

import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Data.VectorSpace

-- |A spline is a piecewise polynomial vector-valued function.  The necessary
-- and sufficient instance definition is 'toBSpline'.
class (VectorSpace v, Fractional (Scalar v), Ord (Scalar v)) => Spline s v where
    -- |Returns the domain of a spline.  In the case of B-splines, this is
    -- the domain on which a spline with this degree and knot vector has a 
    -- full basis set.  In other cases, it should be no larger than 
    -- @splineDomain . toBSpline@, but may be smaller.  Within this domain,
    -- 'evalSpline' should agree with @'evalSpline' . 'toBSpline'@ (not 
    -- necessarily exactly, but up to reasonable expectations of numerical 
    -- accuracy).
    splineDomain :: s v -> Maybe (Scalar v, Scalar v)
    splineDomain = knotDomain <$> knotVector <*> splineDegree
    
    evalSpline :: s v -> Scalar v -> v
    evalSpline = evalSpline . toBSpline
    
    splineDegree :: s v -> Int
    splineDegree = splineDegree . toBSpline
    
    knotVector :: s v -> Knots (Scalar v)
    knotVector = knotVector . toBSpline
    
    toBSpline :: s v -> BSpline.BSpline V.Vector v

-- TODO: this class should probably go away.  all it really does is overload something that doesn't really have any implementation-independent semantics (or does it?).
class Spline s v => ControlPoints s v where
    controlPoints :: s v -> V.Vector v

instance (VectorSpace v, Fractional (Scalar v), Ord (Scalar v)) => Spline (BSpline.BSpline V.Vector) v where
    evalSpline = BSpline.evalBSpline
    splineDegree = BSpline.degree
    knotVector = BSpline.knotVector
    toBSpline = id

instance ( VectorSpace a, Fractional (Scalar a), Ord (Scalar a), G.Vector v a
         , G.Vector v (Scalar a)) => Spline (BSpline.BSpline v) a where
    evalSpline = BSpline.evalBSpline
    splineDegree = BSpline.degree
    knotVector = BSpline.knotVector
    toBSpline (BSpline.Spline deg ks ctp) = BSpline.Spline deg ks (G.convert $ ctp)

instance Spline (BSpline.BSpline V.Vector) a => ControlPoints (BSpline.BSpline V.Vector) a where
    controlPoints = BSpline.controlPoints

instance (Spline (BSpline.BSpline v) a, G.Vector v a) => ControlPoints (BSpline.BSpline v) a where
    controlPoints = V.convert . BSpline.controlPoints