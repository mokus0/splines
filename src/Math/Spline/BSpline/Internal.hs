{-# LANGUAGE FlexibleContexts #-}
module Math.Spline.BSpline.Internal
    (BSpline(..), mapControlPoints, evalBSpline, insertKnot, deBoor) where

import Math.Spline.Knots

import Data.List (zipWith4)
import Data.Monoid
import Data.VectorSpace

data BSpline v = Spline
    { degree        :: !Int
    , knotVector    :: Knots (Scalar v)
    , controlPoints :: [v]
    }

mapControlPoints f spline = spline
    { controlPoints = map f (controlPoints spline)
    , knotVector = knotVector spline
    }

evalBSpline spline = head . last . deBoor spline

-- |Insert one knot into a 'BSpline'
insertKnot
  :: (VectorSpace a, Ord (Scalar a), Fractional (Scalar a)) =>
     BSpline a -> Scalar a -> BSpline a
insertKnot spline x = spline
    { knotVector    = knotVector spline `mappend` knot x
    , controlPoints = zipWith4 (interp x) us (drop p us) ds (tail ds)
    }
    where
        us = knots (knotVector spline)
        p  = degree spline
        ds = extend (controlPoints spline)


-- duplicate the endpoints of a list; for example,
-- extend [1..5] -> [1,1,2,3,4,5,5]
extend []       = []
extend (x:xs)   = x : extend' x xs
    where   extend' x []      = [x,x]
            extend' x (x':xs) = x:   extend' x' xs

deBoor spline x = go us (controlPoints spline)
    where
        us = knots (knotVector spline)
        
        -- Upper endpoints of the intervals are the same for
        -- each row in the table (they just line up differently
        -- with the lower endpoints):
        uHi = drop (degree spline + 1) us
        
        -- On each pass, the lower endpoints of the 
        -- interpolation intervals advance and the new 
        -- coefficients are given by linear interpolation
        -- on the current intervals:
        go       _ [] = []
        go (_:uLo) ds = ds : go uLo ds'
            where
                ds' = zipWith4 (interp x) uLo uHi
                                          ds (tail ds)

interp x x0 x1 y0 y1
    |  x <  x0  = y0
    |  x >= x1  = y1
    | otherwise = lerp y0 y1 a
    where
        a = (x - x0) / (x1 - x0)

