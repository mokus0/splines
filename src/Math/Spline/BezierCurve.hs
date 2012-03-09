{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Math.Spline.BezierCurve
    ( BezierCurve, bezierCurve, splitBezierCurve
    , evalSpline
    ) where

import Math.Spline.BSpline
import Math.Spline.Class
import Math.Spline.Knots

import Control.Applicative
import qualified Data.Vector.Safe as V
import Data.VectorSpace

-- |A BezierCurve curve on @0 <= x <= 1@.
data BezierCurve t = BezierCurve !Int !(V.Vector t) deriving (Eq, Ord)

-- |Construct a Bezier curve from a list of control points.  The degree
-- of the curve is one less than the number of control points.
bezierCurve :: V.Vector t -> BezierCurve t
bezierCurve cs
    | V.null cs = error "bezierCurve: no control points given"
    | otherwise = BezierCurve (V.length cs - 1) cs

instance Show v => Show (BezierCurve v) where
    showsPrec p (BezierCurve _ cs) = showParen (p>10)
        ( showString "bezierCurve "
        . showsPrec 11 cs
        )

instance (VectorSpace v, Fractional (Scalar v), Ord (Scalar v)) => Spline BezierCurve v where
    splineDomain (BezierCurve _  _) = Just (0,1)
    evalSpline   (BezierCurve _ cs) = V.head . last . deCasteljau cs
    splineDegree (BezierCurve p  _) = p
    knotVector   (BezierCurve p  _) = fromList [(0, p+1), (1, p+1)]
    toBSpline = bSpline <$> knotVector <*> controlPoints

instance Spline BezierCurve v => ControlPoints BezierCurve v where
    controlPoints (BezierCurve _ cs) = cs

deCasteljau :: VectorSpace v => V.Vector v -> Scalar v -> [V.Vector v]
deCasteljau cs t
    | V.null cs = []
    | otherwise = cs : deCasteljau (V.zipWith interp cs (V.tail cs)) t
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
    ( BezierCurve n (V.fromList (map V.head css))
    , BezierCurve n (V.reverse (V.fromList (map V.last css)))
    ) where css = deCasteljau cs t
