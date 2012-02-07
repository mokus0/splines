{-# LANGUAGE FlexibleContexts #-}
module Math.Spline.BSpline.Internal
    ( BSpline(..)
    , mapControlPoints
    , evalBSpline
    , evalNaturalBSpline
    , evalReferenceBSpline
    , insertKnot
    , deBoor
    , slice
    ) where

import Math.Spline.Knots
import Math.Spline.BSpline.Reference (bases)

import Data.Monoid
import Data.Ratio
import Data.Vector as V hiding (slice)
import Data.VectorSpace
import Prelude as P

data BSpline t = Spline
    { degree        :: !Int
    , knotVector    :: Knots (Scalar t)
    , controlPoints :: Vector t
    }

mapControlPoints f spline = spline
    { controlPoints = V.map f (controlPoints spline)
    , knotVector = knotVector spline
    }

evalBSpline spline = V.head . P.last . deBoor spline
evalNaturalBSpline spline x = V.head (P.last (deBoor (slice spline x) x))
evalReferenceBSpline (Spline deg kts cps) x = P.sum (P.zipWith (*) (bases kts x !! deg) (V.toList cps))

-- |Insert one knot into a 'BSpline'
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
extend vec
    | V.null vec    = V.empty
    | otherwise     = V.cons (V.head vec) (V.snoc vec (V.last vec))

-- The table from de Boor's algorithm, calculated for the entire spline.  If that is not necessary
-- (for example, if you are only evaluating the spline), then use 'slice' on the spline first.
-- 'splitBSpline' currently uses the whole table.  It is probably not necessary there, but it 
-- greatly simplifies the definition and makes the similarity to splitting Bezier curves very obvious.
deBoor spline x = go us (controlPoints spline)
    where
        us = knotsVector (knotVector spline)
        -- Upper endpoints of the intervals are the same for
        -- each row in the table (they just line up differently
        -- with the lower endpoints):
        uHi = V.drop (degree spline + 1) us

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

interp x x0 x1 y0 y1
    |  x <  x0  = y0
    |  x >= x1  = y1
    | otherwise = lerp y0 y1 a
    where
        a = (x - x0) / (x1 - x0)

-- "slice" a spline to contain only those knots and control points that 
-- actually influence the value at 'x'
--
-- It should be true for any valid BSpline that:
-- degree (slice f x) == degree f
-- slice (slice f x) x == slice f x
-- {x in domain of f} => {x in domain of slice f x}
-- {x in domain of f} => evalBSpline (slice f x) x == evalBSpline f x
slice spline x = spline
    { knotVector    = stakeKnots (n + n) . sdropKnots (l - n) $ knotVector spline
    , controlPoints = vtake       n      . vdrop      (l - n) $ controlPoints spline
    }
    where
        l = maybe 0 id $ V.findIndex (> x) us
        n = degree spline + 1
        
        us = knotsVector (knotVector spline)

-- Try to take n, but if there's not enough, pad the rest with 0s
vtake n xs
    | n <= V.length xs = V.take n xs
    | otherwise = xs V.++ V.replicate (n - V.length xs) zeroV

-- Try to drop n, but if n is negative, pad the beginning with 0s
vdrop n xs
    | n >= 0 = V.drop n xs
    | otherwise = V.replicate (-n) zeroV V.++ xs

-- Try to take n knots, but if there aren't enough, increase the multiplicity of the last knot
stakeKnots n kts
    | n <= nKts = takeKnots n kts
    | otherwise = case maxKnot kts of
        Nothing     -> multipleKnot 0 (n - nKts)
        Just (k, m) -> setKnotMultiplicity k (m + n - nKts) kts
    where nKts = numKnots kts

-- Try to drop n knots, but if n is negative, increase the multiplicity of the first knot by @abs n@
sdropKnots n kts
    | n >= 0    = dropKnots n kts
    | otherwise = case minKnot kts of
        Nothing     -> multipleKnot 0 (-n)
        Just (k, m) -> setKnotMultiplicity k (m - n) kts

