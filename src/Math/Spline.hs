module Math.Spline
    ( Spline(..)
    
    , Knots, mkKnots, knots
    
    , Bezier, bezier
    , BSpline, bSpline
    , MSpline, mSpline, toMSpline
    , ISpline, iSpline, toISpline
    ) where

import Math.Spline.Class
import Math.Spline.Knots
import Math.Spline.Bezier
import Math.Spline.BSpline
import Math.Spline.MSpline
import Math.Spline.ISpline
