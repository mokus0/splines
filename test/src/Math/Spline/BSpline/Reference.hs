{-# LANGUAGE ParallelListComp #-}
-- |Reference implementation of B-Splines; very inefficient but \"obviously\"
-- correct.
module Math.Spline.BSpline.Reference where

import Math.Spline.Knots
import Math.Polynomial (Poly)
import qualified Math.Polynomial as Poly

bases :: (Fractional a, Ord a) => Knots a -> a -> [[a]]
bases kts x = coxDeBoor kts interp initial
    where
        ind True  = 1
        ind False = 0

        initial = 
            [ ind (t_j <= x && x < t_jp1)
            | (t_j, t_jp1) <- knotSpans kts 1
            ]
        interp t_j d0 b_nm1_j t_jpnp1 d1 b_nm1_jp1
            = (if d0 == 0 then 0 else (x       - t_j) / d0) * b_nm1_j
            + (if d1 == 0 then 0 else (t_jpnp1 -   x) / d1) * b_nm1_jp1

basisFunctions :: (Fractional a, Ord a) => Knots a -> [[a -> a]]
basisFunctions kts = coxDeBoor kts interp initial
    where
        ind True  = 1
        ind False = 0

        initial = 
            [ \x -> ind (t_j <= x && x < t_jp1)
            | (t_j, t_jp1) <- knotSpans kts 1
            ]
        interp t_j d0 b_nm1_j t_jpnp1 d1 b_nm1_jp1 x
            = (if d0 == 0 then 0 else (x       - t_j)     / d0) * b_nm1_j   x
            + (if d1 == 0 then 0 else (t_jpnp1 -   x) / d1) * b_nm1_jp1 x

basisPolynomialsAt :: (Fractional a, Ord a) => Knots a -> a -> [[Poly a]]
basisPolynomialsAt kts x = coxDeBoor kts interp initial
    where
        ind True  = Poly.one
        ind False = Poly.zero
        
        initial = 
            [ ind (t_j <= x && x < t_jp1)
            | (t_j, t_jp1) <- knotSpans kts 1
            ]
        interp t_j d0 b_nm1_j t_jpnp1 d1 b_nm1_jp1
            = (if d0 == 0 then Poly.zero else (Poly.x                 - Poly.constPoly t_j) / d0) * b_nm1_j
            + (if d1 == 0 then Poly.zero else (Poly.constPoly t_jpnp1 -             Poly.x) / d1) * b_nm1_jp1
            where
                infixl 6 +, -
                p + q   = Poly.addPoly p q
                p - q   = p + (Poly.negatePoly q)
                
                infixl 7 *, /
                p * q   = Poly.multPoly p q
                p / s   = Poly.scalePoly (recip s) p

-- This is a straightforward implementation of the Cox-De Boor recursion scheme
-- generalized in a slightly strange way; the initial vector is a parameter 
-- and the actual computation of the recursion step is a function parameter.
-- The purpose is to allow the same recursion to be applied when computing basis
-- function values and  basis polynomials.
coxDeBoor kts interp initial = table
    where
        ts = knots kts
        table = initial :
            [ [ interp t_j d0 b_nm1_j t_jpnp1 d1 b_nm1_jp1
              | (b_nm1_j, b_nm1_jp1)    <- spans 1 prevBasis
              | (d0, d1)                <- spans 1 (spanDiffs n ts)
              | (t_j, t_jpnp1)          <- spans (n+1) ts
              ]
            | prevBasis <- takeWhile (not . null) table
            | n <- [1..]
            ]

spans :: Int -> [a] -> [(a,a)]
spans     = spansWith (,)
spanDiffs :: Num a => Int -> [a] -> [a]
spanDiffs = spansWith subtract
spansWith f n ts = zipWith f ts (drop n ts)
