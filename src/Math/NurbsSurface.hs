{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Math.NurbsSurface where

import Data.List
import Data.Maybe
import Control.Applicative

import Data.VectorSpace
import Math.Spline.BSpline
import Math.Spline.Knots

data NurbsSurface s v = NurbsSurface {
  uKnots :: Knots s,
  vKnots :: Knots s,
  controlPoints :: [[(s, v)]]
  } deriving Show

uDegree :: NurbsSurface s v -> Int
uDegree (NurbsSurface k _ cps) = m - n where
  m = numKnots k - 1
  n = length cps

vDegree :: NurbsSurface s v -> Int
vDegree (NurbsSurface _ k cps) = m - n where
  m = numKnots k - 1
  n = length $ head cps

unH :: (Fractional (Scalar v), VectorSpace v) => (Scalar v, v) -> v
unH (w,v) = recip w *^ v

toH :: VectorSpace v => Scalar v -> v -> (Scalar v, v)
toH w v = (w, w *^ v)

-- | @evalSurface n u v@ is the point on the surface n with
--   parametric coördinates u, v
evalSurface :: (VectorSpace v, s ~ Scalar v, Fractional s, s ~ Scalar s, Ord s, AdditiveGroup s, VectorSpace s) =>
               NurbsSurface s v -> s -> s -> Maybe v
evalSurface n u v = unH <$> evalSurface' n u v

evalSurface' :: (VectorSpace v, s ~ Scalar v, Fractional s, s ~ Scalar s, Ord s, AdditiveGroup s, VectorSpace s) =>
                NurbsSurface s v -> s -> s -> Maybe (s,v)
evalSurface' n u v | isNothing uspan = Nothing
                   | isNothing vspan = Nothing
                   | otherwise = Just . head . head $ rightMult uP (transpose [vFuns]) where
  uspan = findSpan (uKnots n) u
  vspan = findSpan (vKnots n) v
  uDeg  = uDegree n
  vDeg  = vDegree n
  uFuns = basisFuns' uDeg (uKnots n) u
  vFuns = basisFuns' vDeg (vKnots n) v
  leftMult = genMMult (*^) (^+^) zeroV
  rightMult = genMMult (^*) (^+^) zeroV
  rows = take (uDeg + 1) $ drop (fromJust uspan - uDeg) $ controlPoints n
  cps = map (take (vDeg + 1) . drop (fromJust vspan - vDeg)) rows
  uP    = leftMult [uFuns] cps

-- | surfaceGrid evaluates the NurbsSurface over a grid of points.
--   The grid is uniformly spaced (on each axis) in u, v, but not, in general,
--   in R3.
surfaceGrid :: (s ~ Scalar v, s ~ Scalar s, Fractional s, AdditiveGroup s, VectorSpace s, Enum s, VectorSpace v, Ord s)
               => NurbsSurface s v -- ^ surface to be evaluated
               -> Int           -- ^ number of points to evaluate on first (u) axis
               -> Int           -- ^ number of points to evaluate on second (v) axis
               -> [[v]]        -- ^ each inner list shares a value of u
surfaceGrid n uCt vCt = map f us where
  f u = mapMaybe (evalSurface n u) vs
  us = ctRange (uKnots n) (uDegree n) uCt
  vs = ctRange (vKnots n) (vDegree n) vCt
  ctRange ks p ct = case knotDomain ks p of
    Nothing       -> []
    Just (lo, hi) -> [lo, lo+(hi-lo)/(fromIntegral ct - 1)..hi]

-- | Generalized matrix matrix multiply
genMMult  :: (a -> b -> c)
        -> (c -> c -> c)
        -> c
        -> [[a]]
        -> [[b]]
        -> [[c]]
genMMult mul add c0 arr brr =
  [[foldr add c0 $ zipWith mul a b | b <- transpose brr] | a <- arr]
