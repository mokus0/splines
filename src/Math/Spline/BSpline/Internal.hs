{-# LANGUAGE FlexibleContexts #-}
module Math.Spline.BSpline.Internal
    (BSpline(..), mapControlPoints, evalBSpline, insertKnot, deBoor) where

import Math.Spline.Knots

import Data.Monoid
import Data.Ratio
import Data.Vector as V
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

deBoor spline x = go uLo . vtake (deg + 1) . vdrop (l - deg) $
                  controlPoints spline
    where
        l = maybe (-1) pred $ V.findIndex (> x) us
        deg = degree spline
        zero = fromRational (0 % 1)

        us = knotsVector (knotVector spline)
        uLo = stake (deg + 1) $ sdrop (l - deg) $ us
        -- Upper endpoints of the intervals are the same for
        -- each row in the table (they just line up differently
        -- with the lower endpoints):
        uHi = sdrop (l + 1) us

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

        -- Try to take n, but if there's not enough, pan the rest with 0s
        vtake n xs
            | n <= V.length xs = V.take n xs
            | otherwise = xs V.++ V.replicate (n - V.length xs) zeroV
        stake n xs
            | n <= V.length xs = V.take n xs
            | otherwise = xs V.++ V.replicate (n - V.length xs) 0

        -- Try to drop n, but if n is negative, pan the beginning with 0s
        vdrop n xs
            | n >= 0 = V.drop n xs
            | otherwise = V.replicate (-n) zeroV V.++ xs
        sdrop n xs
            | n >= 0 = V.drop n xs
            | otherwise = V.replicate (-n) 0 V.++ xs

interp x x0 x1 y0 y1
    |  x <  x0  = y0
    |  x >= x1  = y1
    | otherwise = lerp y0 y1 a
    where
        a = (x - x0) / (x1 - x0)

