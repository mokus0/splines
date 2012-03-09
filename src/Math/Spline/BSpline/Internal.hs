{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Math.Spline.BSpline.Internal
    ( BSpline(..)
    , mapControlPoints
    , evalBSpline
    , evalNaturalBSpline
    , insertKnot
    , deBoor
    , slice
    ) where

import Math.Spline.Knots

import Data.Monoid
import Data.Vector.Safe as V hiding (slice)
import Data.VectorSpace
import Prelude as P

-- | A B-spline, defined by a knot vector (see 'Knots') and a sequence of control points.
data BSpline t = Spline
    { degree        :: !Int
    , knotVector    :: Knots (Scalar t)
    , controlPoints :: Vector t
    }

deriving instance (Eq   (Scalar v), Eq   v) => Eq   (BSpline v)
deriving instance (Ord  (Scalar v), Ord  v) => Ord  (BSpline v)
instance (Show (Scalar v), Show v) => Show (BSpline v) where
    showsPrec p (Spline _ kts cps) = showParen (p>10) 
        ( showString "bSpline "
        . showsPrec 11 kts
        . showChar ' '
        . showsPrec 11 cps
        )

mapControlPoints :: (Scalar a ~ Scalar b) => (a -> b) -> BSpline a -> BSpline b
mapControlPoints f spline = spline
    { controlPoints = V.map f (controlPoints spline)
    , knotVector = knotVector spline
    }

-- |Evaluate a B-spline at the given point.  This uses a slightly modified version of 
-- de Boor's algorithm which is only strictly correct inside the domain of the spline.
-- Unlike the standard algorithm, the basis functions always sum to 1, even outside the
-- domain of the spline.  This is mainly useful for \"clamped\" splines - the values at
-- or outside the endpoints will always be the value of the nearest control point.
--
-- For a standard implementation of de Boor's algorithm, see 'evalNaturalBSpline'.
-- For a (much slower) strictly mathematically correct evaluation, see 'evalReferenceBSpline'.
evalBSpline :: (VectorSpace v, Fractional (Scalar v), Ord (Scalar v))
     => BSpline v -> Scalar v -> v
evalBSpline spline
     | V.null (controlPoints spline) = zeroV
     | otherwise = V.head . P.last . deBoor spline

-- | Evaluate a B-spline at the given point.  This uses de Boor's algorithm, which is 
-- only strictly correct inside the domain of the spline.
-- 
-- For a (much slower) strictly mathematically correct evaluation, see 'evalReferenceBSpline'.
evalNaturalBSpline :: (VectorSpace v, Fractional (Scalar v), Ord (Scalar v)) 
    => BSpline v -> Scalar v -> v
evalNaturalBSpline spline x = V.head (P.last (deBoor (slice spline x) x))

-- |Insert one knot into a 'BSpline' without changing the spline's shape.
insertKnot
  :: (VectorSpace a, Ord (Scalar a), Fractional (Scalar a)) =>
     BSpline a -> Scalar a -> BSpline a
insertKnot spline x = spline
    { knotVector    = knotVector spline `mappend` knot x
    , controlPoints = V.zipWith4 (interp x) us (V.drop p us) ds (V.tail ds)
    }
    where
        us = knotsVector (knotVector spline)
        p  = degree spline
        ds = extend (controlPoints spline)

-- duplicate the endpoints of a list; for example,
-- extend [1..5] -> [1,1,2,3,4,5,5]
extend :: Vector t -> Vector t
extend vec
    | V.null vec    = V.empty
    | otherwise     = V.cons (V.head vec) (V.snoc vec (V.last vec))

-- | The table from de Boor's algorithm, calculated for the entire spline.  If that is not necessary
-- (for example, if you are only evaluating the spline), then use 'slice' on the spline first.
-- 'splitBSpline' currently uses the whole table.  It is probably not necessary there, but it 
-- greatly simplifies the definition and makes the similarity to splitting Bezier curves very obvious.
deBoor :: (Fractional (Scalar a), Ord (Scalar a), VectorSpace a)
    => BSpline a -> Scalar a -> [Vector a]
deBoor spline x = go us0 (controlPoints spline)
    where
        us0 = knotsVector (knotVector spline)
        -- Upper endpoints of the intervals are the same for
        -- each row in the table (they just line up differently
        -- with the lower endpoints):
        uHi = V.drop (degree spline + 1) us0

        -- On each pass, the lower endpoints of the
        -- interpolation intervals advance and the new
        -- coefficients are given by linear interpolation
        -- on the current intervals:
        go us ds
            | V.null ds = []
            | otherwise = ds : go uLo ds'
            where
                uLo = V.tail us
                ds' = V.zipWith4 (interp x) uLo uHi
                                            ds (V.tail ds)

interp :: (Fractional (Scalar v), Ord (Scalar v), VectorSpace v)
    => Scalar v -> Scalar v -> Scalar v -> v -> v -> v
interp x x0 x1 y0 y1
    |  x <  x0  = y0
    |  x >= x1  = y1
    | otherwise = lerp y0 y1 a
    where
        a = (x - x0) / (x1 - x0)

-- "slice" a spline to contain only those knots and control points that 
-- actually influence the value at 'x'.
--
-- It should be true for any valid BSpline that:
-- degree (slice f x) == degree f
-- slice (slice f x) x == slice f x
-- {x in domain of f} => {x in domain of slice f x}
-- {x in domain of f} => evalBSpline (slice f x) x == evalBSpline f x
slice :: (Num (Scalar v), Ord (Scalar v), AdditiveGroup v)
     => BSpline v -> Scalar v -> BSpline v
slice spline x = spline
    { knotVector    = stakeKnots (n + n) . sdropKnots (l - n) $ knotVector spline
    , controlPoints = vtake       n      . vdrop      (l - n) $ controlPoints spline
    }
    where
        l = maybe 0 id $ V.findIndex (> x) us
        n = degree spline + 1
        
        us = knotsVector (knotVector spline)

-- Try to take n, but if there's not enough, pad the rest with 0s
vtake :: AdditiveGroup t => Int -> Vector t -> Vector t
vtake n xs
    | n <= V.length xs = V.take n xs
    | otherwise = xs V.++ V.replicate (n - V.length xs) zeroV

-- Try to drop n, but if n is negative, pad the beginning with 0s
vdrop :: AdditiveGroup t => Int -> Vector t -> Vector t
vdrop n xs
    | n >= 0 = V.drop n xs
    | otherwise = V.replicate (-n) zeroV V.++ xs

-- Try to take n knots, but if there aren't enough, increase the multiplicity of the last knot
stakeKnots :: (Num k, Ord k) => Int -> Knots k -> Knots k
stakeKnots n kts
    | n <= nKts = takeKnots n kts
    | otherwise = case maxKnot kts of
        Nothing     -> multipleKnot 0 (n - nKts)
        Just (k, m) -> setKnotMultiplicity k (m + n - nKts) kts
    where nKts = numKnots kts

-- Try to drop n knots, but if n is negative, increase the multiplicity of the first knot by @abs n@
sdropKnots :: (Num k, Ord k) => Int -> Knots k -> Knots k
sdropKnots n kts
    | n >= 0    = dropKnots n kts
    | otherwise = case minKnot kts of
        Nothing     -> multipleKnot 0 (-n)
        Just (k, m) -> setKnotMultiplicity k (m - n) kts

