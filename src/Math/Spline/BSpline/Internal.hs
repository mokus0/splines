module Math.Spline.BSpline.Internal where

import Math.Spline.Knots

import Data.List (zipWith4)
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

