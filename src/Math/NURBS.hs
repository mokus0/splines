{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances, TypeFamilies #-}
module Math.NURBS
    ( NURBS
    , nurbs, toNURBS
    , evalNURBS, nurbsDomain
    , nurbsDegree, nurbsKnotVector, nurbsControlPoints
    , splitNURBS
    ) where

import qualified Data.Vector as V
import Data.VectorSpace hiding (project)
import Math.Spline.Class (Spline, toBSpline)
import Math.Spline.BSpline.Internal
import Math.Spline.BSpline
import Math.Spline.Knots

newtype NURBS v = NURBS (BSpline (Scalar v, v))

deriving instance (Eq   v, Eq   (Scalar v), Eq   (Scalar (Scalar v))) => Eq   (NURBS v)
deriving instance (Ord  v, Ord  (Scalar v), Ord  (Scalar (Scalar v))) => Ord  (NURBS v)
instance (Show v, Show (Scalar v), Show (Scalar (Scalar v))) => Show (NURBS v) where
    showsPrec p (NURBS spline) = showParen (p>11)
        ( showString "nurbs "
        . showsPrec 11 spline
        )

toNURBS :: (Spline s v, Scalar v ~ Scalar (Scalar v)) => s v -> NURBS v
toNURBS = NURBS . mapControlPoints (\p -> (1,p)) . toBSpline

nurbs :: (VectorSpace v, Scalar v ~ w,
          VectorSpace w, Scalar w ~ w)
       => Knots (Scalar v) -> V.Vector (w, v) -> NURBS v
nurbs kts cps = NURBS (bSpline kts cps)

-- |Constructs the homogeneous-coordinates B-spline that corresponds to this
-- NURBS curve
nurbsAsSpline :: VectorSpace v => NURBS v -> BSpline (Scalar v, v)
nurbsAsSpline (NURBS spline) = spline 
    { controlPoints = V.map homogenize (controlPoints spline) }
    where
        homogenize (w,v) = (w, v ^* w)

-- |Constructs the NURBS curve corresponding to a homogeneous-coordinates B-spline
splineAsNURBS :: (VectorSpace v, Fractional (Scalar v)) => BSpline (Scalar v, v) -> NURBS v
splineAsNURBS spline = NURBS spline 
    { controlPoints = V.map unHomogenize (controlPoints spline) }
    where
        unHomogenize (w,v) = (w, v ^/ w)


evalNURBS
  :: (VectorSpace v, Scalar v ~ w,
      VectorSpace w, Scalar w ~ w,
      Fractional w, Ord w) =>
     NURBS v -> w -> v
evalNURBS f = project . evalBSpline (nurbsAsSpline f)
    where
        project (w,v) = recip w *^ v


-- |Returns the domain of a NURBS - that is, the range of parameter values
-- over which a spline with this degree and knot vector has a full basis set.
nurbsDomain :: Scalar v ~ Scalar (Scalar v) => 
    NURBS v -> Maybe (Scalar v, Scalar v)
nurbsDomain (NURBS spline) = knotDomain (knotVector spline) (degree spline)

nurbsDegree :: NURBS v -> Int
nurbsDegree (NURBS spline) = degree spline

nurbsKnotVector :: Scalar v ~ Scalar (Scalar v) => NURBS v -> Knots (Scalar v)
nurbsKnotVector (NURBS spline) = knotVector spline

nurbsControlPoints :: NURBS v -> V.Vector (Scalar v, v)
nurbsControlPoints (NURBS spline) = controlPoints spline

splitNURBS :: (VectorSpace v, Scalar v ~ w,
               VectorSpace w, Scalar w ~ w,
               Ord w, Fractional w)
    => NURBS v -> Scalar v -> Maybe (NURBS v, NURBS v)
splitNURBS f t = do
    (s0, s1) <- splitBSpline (nurbsAsSpline f) t
    return (splineAsNURBS s0, splineAsNURBS s1)
