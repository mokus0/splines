{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances, TypeFamilies #-}
module Math.NURBS
    ( NURBS
    , nurbs, toNURBS
    , evalNURBS, nurbsDomain
    , nurbsDegree, nurbsKnotVector, nurbsControlPoints
    , splitNURBS
    ) where

import Data.VectorSpace
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
       => Knots (Scalar v) -> [(w, v)] -> NURBS v
nurbs kts cps = NURBS (bSpline kts cps)

-- |Constructs the homogeneous-coordinates B-spline that corresponds to this
-- NURBS curve
nurbsAsSpline (NURBS spline) = spline 
    { controlPoints = map homogenize (controlPoints spline) }
    where
        homogenize (w,v) = (w, w *^ v)

-- |Constructs the NURBS curve corresponding to a homogeneous-coordinates B-spline
splineAsNURBS spline = NURBS spline 
    { controlPoints = map unHomogenize (controlPoints spline) }
    where
        unHomogenize (w,v) = (w, recip w *^ v)


evalNURBS
  :: (VectorSpace v, Scalar v ~ w,
      VectorSpace w, Scalar w ~ w,
      Fractional w, Ord w) =>
     NURBS v -> w -> v
evalNURBS nurbs = project . evalBSpline (nurbsAsSpline nurbs)
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

nurbsControlPoints :: NURBS v -> [(Scalar v, v)]
nurbsControlPoints (NURBS spline) = controlPoints spline

splitNURBS :: (VectorSpace v, Scalar v ~ w,
               VectorSpace w, Scalar w ~ w,
               Ord w, Fractional w)
    => NURBS v -> Scalar v -> Maybe (NURBS v, NURBS v)
splitNURBS nurbs t = do
    (s0, s1) <- splitBSpline (nurbsAsSpline nurbs) t
    return (splineAsNURBS s0, splineAsNURBS s1)
