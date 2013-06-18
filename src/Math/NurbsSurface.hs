{-# LANGUAGE TypeFamilies #-}

module Math.NurbsSurface where

import Data.List
import Data.Maybe
import Control.Applicative

import Data.VectorSpace
import Math.Spline.BSpline
import Math.Spline.Knots

-- TODO generalize types below to remove R3 dependency

data NurbsSurface = NurbsSurface {
  uKnots :: Knots Double,
  vKnots :: Knots Double,
  controlPoints :: [[Homogeneous]]
  } deriving Show

uDegree :: NurbsSurface -> Int
uDegree (NurbsSurface k _ cps) = m - n where
  m = numKnots k - 1
  n = length cps

vDegree :: NurbsSurface -> Int
vDegree (NurbsSurface _ k cps) = m - n where
  m = numKnots k - 1
  n = length $ head cps

type Pt = (Double, Double, Double)

data Homogeneous = H Double Pt
                 deriving Show

unH :: Homogeneous -> Pt
unH (H w r) = recip w *^ r

toH :: Double -> Pt -> Homogeneous
toH w pt = H w (w *^ pt)

instance VectorSpace Homogeneous where
  type Scalar Homogeneous = Double
  s *^ H w v = H (s*w) (s *^ v)  -- TODO, H (w/s) v when appropriate

instance AdditiveGroup Homogeneous where
  zeroV = H 0 zeroV
  H w0 r0 ^+^ H w1 r1 = H (w0 + w1) (r0 ^+^ r1)
  negateV (H w r) = H (-1 * w) ((-1) *^ r)


-- | @evalSurface n u v@ is the point on the surface n with
--   parametric coördinates u, v
evalSurface :: NurbsSurface -> Double -> Double -> Maybe Pt
evalSurface n u v = unH <$> evalSurface' n u v

evalSurface' :: NurbsSurface -> Double -> Double -> Maybe Homogeneous
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
surfaceGrid :: NurbsSurface -- ^ surface to be evaluated
               -> Int       -- ^ number of points to evaluate on first (u) axis
               -> Int       -- ^ number of points to evaluate on second (v) axis
               -> [[Pt]]    -- ^ each inner list shares a value of u
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
