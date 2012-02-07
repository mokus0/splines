{-# LANGUAGE ParallelListComp #-}
-- |Reference implementation of B-Splines; very inefficient but \"obviously\"
-- correct.
module Math.Spline.BSpline.Reference
    ( bases
    , basisFunctions
    , basisPolynomials
    , basisPolynomialsAt
    ) where

import Math.Spline.Knots
import Math.Polynomial (Poly)
import qualified Math.Polynomial as Poly

ind :: Num a => Bool -> a
ind True  = 1
ind False = 0

bases :: (Fractional a, Ord a) => Knots a -> a -> [[a]]
bases kts x = coxDeBoor interp initial kts
    where
        initial = 
            [ ind (t_j <= x && x < t_jp1)
            | (t_j, t_jp1) <- knotSpans kts 1
            ]
        interp t_j d0 b_nm1_j t_jpnp1 d1 b_nm1_jp1
            = (if d0 == 0 then 0 else (x       - t_j) / d0) * b_nm1_j
            + (if d1 == 0 then 0 else (t_jpnp1 -   x) / d1) * b_nm1_jp1

-- Alternate version constructing table of functions rather than computing
-- table of values
basisFunctions :: (Fractional a, Ord a) => Knots a -> [[a -> a]]
basisFunctions kts = coxDeBoor interp initial kts
    where
        initial = 
            [ \x -> ind (t_j <= x && x < t_jp1)
            | (t_j, t_jp1) <- knotSpans kts 1
            ]
        interp t_j d0 b_nm1_j t_jpnp1 d1 b_nm1_jp1 x
            = (if d0 == 0 then 0 else (x       - t_j) / d0) * b_nm1_j   x
            + (if d1 == 0 then 0 else (t_jpnp1 -   x) / d1) * b_nm1_jp1 x

-- compute all the basis polynomials for a knot vector, ordered by knot span.
basisPolynomials :: (Fractional a, Ord a) => Knots a -> [[[Poly a]]]
basisPolynomials kts
    | isEmpty kts   = []
    | otherwise     = [basisPolynomialsAt kts kt | kt <- init (distinctKnots kts)]

-- compute all the basis polynomials for the knot span containing a given location.
basisPolynomialsAt :: (Fractional a, Ord a) => Knots a -> a -> [[Poly a]]
basisPolynomialsAt kts x = coxDeBoor interp initial kts
    where
        indPoly True  = Poly.one
        indPoly False = Poly.zero
        
        initial = 
            [ indPoly (t_j <= x && x < t_jp1)
            | (t_j, t_jp1) <- knotSpans kts 1
            ]
        interp t_j d0 b_nm1_j t_jpnp1 d1 b_nm1_jp1
            =   (if d0 == 0 then Poly.zero else (Poly.x                 ^-^ Poly.constPoly t_j) ^/ d0) ^*^ b_nm1_j
            ^+^ (if d1 == 0 then Poly.zero else (Poly.constPoly t_jpnp1 ^-^             Poly.x) ^/ d1) ^*^ b_nm1_jp1
            where
                infixl 6 ^+^, ^-^
                p ^+^ q   = Poly.addPoly p q
                p ^-^ q   = p ^+^ (Poly.negatePoly q)
                
                infixl 7 ^*^, ^/
                p ^*^ q   = Poly.multPoly p q
                p ^/  s   = Poly.scalePoly (recip s) p

-- This is a straightforward implementation of the Cox-De Boor recursion scheme
-- generalized in a slightly strange way; the initial vector is a parameter 
-- and the actual computation of the recursion step is a function parameter.
-- The purpose is to allow the same recursion to be applied when computing basis
-- function values and  basis polynomials.
coxDeBoor :: Num a => (a -> a -> b -> a -> a -> b -> b) -> [b] -> Knots a -> [[b]]
coxDeBoor interp initial kts = table
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

spansWith :: (a -> a -> b) -> Int -> [a] -> [b]
spansWith f n ts = zipWith f ts (drop n ts)
