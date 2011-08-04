{-# LANGUAGE FlexibleContexts #-}
module Math.Spline.BSpline.Internal
    (BSpline(..), mapControlPoints, evalBSpline, insertKnot, deBoor) where

import Math.Spline.Knots

import Data.Monoid
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

