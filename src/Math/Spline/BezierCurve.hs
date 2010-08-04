{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Math.Spline.BezierCurve
    ( BezierCurve, bezierCurve, splitBezierCurve
    , evalSpline
    ) where

import Math.Spline.BSpline
import Math.Spline.Class
import Math.Spline.Knots

import Control.Applicative
import Data.VectorSpace

-- |A BezierCurve curve on @0 <= x <= 1@.
data BezierCurve v = BezierCurve !Int [v] deriving (Eq, Ord)

-- |Construct a Bezier curve from a list of control points.  The degree
-- of the curve is one less than the number of control points.
bezierCurve :: [v] -> BezierCurve v
bezierCurve cs
    | null cs   = error "bezierCurve: no control points given"
    | otherwise = BezierCurve (length cs - 1) cs

instance Show v => Show (BezierCurve v) where
    showsPrec p (BezierCurve _ cs) = showParen (p>10)
        ( showString "bezierCurve "
        . showsPrec 11 cs
        )

instance (VectorSpace v, Fractional (Scalar v), Ord (Scalar v)) => Spline BezierCurve v where
    splineDomain (BezierCurve _  _) = Just (0,1)
    evalSpline   (BezierCurve _ cs) = head . last . deCasteljau cs
    splineDegree (BezierCurve p  _) = p
    knotVector   (BezierCurve p  _) = fromList [(0, p+1), (1, p+1)]
    toBSpline = bSpline <$> knotVector <*> controlPoints

instance Spline BezierCurve v => ControlPoints BezierCurve v where
    controlPoints (BezierCurve _ cs) = cs

deCasteljau :: VectorSpace v => [v] -> Scalar v -> [[v]]
deCasteljau [] t = []
deCasteljau cs t = cs : deCasteljau (zipWith interp cs (tail cs)) t
    where
        interp x0 x1 = lerp x0 x1 t

-- |Split and rescale a Bezier curve.  Given a 'BezierCurve' @b@ and a point 
-- @t@, @splitBezierCurve b t@ creates 2 curves @(b1, b2)@ such that (up to 
-- reasonable numerical accuracy expectations):
-- 
-- > evalSpline b1  x    == evalSpline b (x * t)
-- > evalSpline b2 (x-t) == evalSpline b (x * (1-t))
-- 
splitBezierCurve :: VectorSpace v => BezierCurve v -> Scalar v -> (BezierCurve v, BezierCurve v)
splitBezierCurve (BezierCurve n cs) t = 
    ( BezierCurve n (map head css)
    , BezierCurve n (reverse (map last css))
    ) where css = deCasteljau cs t
