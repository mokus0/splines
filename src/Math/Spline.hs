module Math.Spline
    ( Spline(..), ControlPoints(..)
    
    , Knots, mkKnots, knots
    
    , BezierCurve, bezierCurve
    , BSpline, bSpline
    , MSpline, mSpline, toMSpline
    , ISpline, iSpline, toISpline
    , CSpline, cSpline
    ) where

import Math.Spline.Class
import Math.Spline.Knots
import Math.Spline.BezierCurve
import Math.Spline.BSpline
import Math.Spline.MSpline
import Math.Spline.ISpline
import Math.Spline.Hermite
