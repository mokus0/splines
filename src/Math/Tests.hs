{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Math.Tests where

import Test.HUnit
import Test.QuickCheck

import Data.Maybe
import Data.Vector ((!))
import Control.Applicative

import Data.VectorSpace
import Math.Spline.Knots
import Math.Spline.BSpline
import Math.NurbsSurface

class Approx a where
  (~~) :: a -> a -> Bool

instance (Fractional a, Ord a) => Approx a where
  (~~) a b = abs (a - b) < 0.001

instance (Approx a, Approx b) => Approx (a,b) where
  (x0, y0) ~~ (x1, y1) = x0~~x1 && y0~~y1

instance (Approx a, Approx b, Approx c) => Approx (a,b,c) where
  (x0,y0,z0) ~~ (x1,y1,z1) = x0~~x1 && y0~~y1 && z0~~z1

instance Approx a => Approx [a] where
  a ~~ b = and $ zipWith (~~) a b

cyl :: NurbsSurface Double (Double, Double, Double)
cyl = NurbsSurface
      kLinear
      kCircle
      [zipWith toH wts circ, zipWith toH wts $ map (^+^ zhat) circ] where
        zhat = (0,0,1)
        wts = concat. repeat $ [1, sqrt 2 / 2]
        circ = [(1,0,0),   (1,1,0),
                (0,1,0),   (-1,1,0),
                (-1,0,0), (-1,-1,0),
                (0,-1,0), (1,-1,0),
                (1,0,0)]

kLinear = mkKnots [0,0,1,1] -- degree 1

kCircle = mkKnots [0,0,0,0.25,0.25,0.5,0.5,0.75,0.75,1,1,1] -- degree 2

toZAxis (x,y,_) = sqrt (x**2 + y**2)

knotCheck k u n | u < 0 = isNothing n
                | u > 1 = isNothing n
knotCheck k u (Just i) = knotsVector k ! i <= u && knotsVector k ! (i+1) >= u

data UValue = UV Double

instance Show UValue where
  show (UV d) = show d

instance Arbitrary UValue where
  arbitrary = UV <$> choose (0,1)

data SaneN = SaneN Int

instance Show SaneN where
  show (SaneN n) = show n

instance Arbitrary SaneN where
  arbitrary = SaneN <$> choose (2,100)

knotsTest = test [
  "findSpan" ~: [
     "linear" ~: ["0" ~: Just 1 ~=? findSpan kLinear 0
                 ,"0.5" ~: Just 1 ~=? findSpan kLinear 0.5
                 ,"1" ~: Just 1 ~=? findSpan kLinear 1],
     "circle" ~: ["0" ~: Just 2 ~=? findSpan kCircle 0,
                  "0.1" ~: Just 2 ~=? findSpan kCircle 0.1,
                  "0.25" ~: Just 4 ~=? findSpan kCircle 0.25,
                  "0.3" ~: Just 4 ~=? findSpan kCircle 0.3,
                  "0.5" ~: Just 6 ~=? findSpan kCircle 0.5,
                  "0.75" ~: Just 8 ~=? findSpan kCircle 0.75,
                  "1" ~: Just 8 ~=? findSpan kCircle 1]],
  "basisFuns" ~: [
    "linear" ~: ["0" ~: [1,0] ~=? basisFuns' 1 kLinear 0,
                 "0.25" ~: [0.75, 0.25] ~=? basisFuns' 1 kLinear 0.25,
                 "0.5" ~: [0.5,0.5] ~=? basisFuns' 1 kLinear 0.5,
                 "1" ~: [0,1] ~=? basisFuns' 1 kLinear 1],
    "circle" ~: ["0" ~: [1,0,0] ~=? basisFuns' 2 kCircle 0,
                 "0.1" ~: expectedU2 (0.1/0.25) ~=? basisFuns' 2 kCircle 0.1,
                 "0.24" ~: expectedU2 (0.24/0.25) ~=? basisFuns' 2 kCircle 0.24,
                 "0.25" ~: expectedU2 0 ~=? basisFuns' 2 kCircle 0.25
                   ]]]

nurbsSurface = test [
  "evalSurface" ~: [
     "cylinder" ~: ["0,0" ~: Just (1,0,0) ~=? evalSurface cyl 0 0,
                    "1,0" ~: Just (1,0,1) ~=? evalSurface cyl 1 0,
                    "0,0.25" ~: Just (0,1,0) ~=? evalSurface cyl 0 0.25]],
  "surfaceGrid" ~: [
    -- only evaluates at knots
    "cylinder" ~: [assertBool "2,5" $ 
                   [[(1.0,0.0,0.0),(0.0,1.0,0.0),(-1.0,0.0,0.0),
                     (0.0,-1.0,0.0),(1.0,0.0,0.0)],
                    [(1.0,0.0,1.0),(0.0,1.0,1.0),(-1.0,0.0,1.0),
                     (0.0,-1.0,1.0),(1.0,0.0,1.0)]] ~~ surfaceGrid cyl 2 5,
                   -- circle is very non-uniform in v
                   assertBool "2,9" $ [
                     [(1,0,0),(s22,s22,0),(0,1,0),(-s22,s22,0),
                      (-1,0,0),(-s22,-s22,0),(0,-1,0),(s22,-s22,0),(1,0,0)],
                     [(1,0,1),(s22,s22,1),(0,1,1),(-s22,s22,1),
                      (-1,0,1),(-s22,-s22,1),(0,-1,1),(s22,-s22,1),(1,0,1)]]
                     ~~ surfaceGrid cyl 2 9,
                   -- z-axis is linear in u
                   assertBool "3 9" $ [
                     [(1,0,0),(s22,s22,0),(0,1,0),(-s22,s22,0),
                      (-1,0,0),(-s22,-s22,0),(0,-1,0),(s22,-s22,0),(1,0,0)],
                     [(1,0,0.5),(s22,s22,0.5),(0,1,0.5),(-s22,s22,0.5),
                      (-1,0,0.5),(-s22,-s22,0.5),(0,-1,0.5),(s22,-s22,0.5),(1,0,0.5)],
                     [(1,0,1),(s22,s22,1),(0,1,1),(-s22,s22,1),
                      (-1,0,1),(-s22,-s22,1),(0,-1,1),(s22,-s22,1),(1,0,1)]]
                     ~~ surfaceGrid cyl 3 9]]]

u2 n x = (1-x)**(2-n) * x**n

expectedU2 x = [u2 0 x, 2 * u2 1 x, u2 2 x]

s22 :: Double
s22 = sqrt 2 / 2

basisPartitionCheck = quickCheck (\(UV u) -> 1 == (sum $ basisFuns' 1 kCircle u))
uSpanCheck          = quickCheck (\u -> knotCheck kCircle u (findSpan kCircle u))
vSpanCheck          = quickCheck (\u -> knotCheck kLinear u (findSpan kLinear u))
radiusCheck         = quickCheck (\(SaneN n) -> (all.all) ((~~1). toZAxis) $ surfaceGrid cyl n n)
