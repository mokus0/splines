{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleContexts #-}
-- |Reference implementation of B-Splines; very inefficient but \"obviously\"
-- correct.
module Math.Spline.BSpline.Reference
    ( bases
    , basisFunctions
    , basisPolynomials
    , basisPolynomialsAt
    , evalReferenceBSpline
    , fitPolyToBSplineAt
    ) where

import qualified Data.Vector.Generic as V
import Data.VectorSpace (VectorSpace, Scalar, (^*), sumV)
import Math.Spline.Knots
import Math.Spline.BSpline.Internal
import Math.Polynomial (Poly)
import qualified Math.Polynomial as Poly

-- | This is a fairly slow function which computes the value of a B-spline at a given point,
-- using the mathematical definition of B-splines.  It is mainly for testing purposes, as a
-- reference against which the other evaluation functions are checked.
evalReferenceBSpline :: (VectorSpace a, Fractional (Scalar a), Ord (Scalar a), V.Vector v a) 
    => BSpline v a -> Scalar a -> a
evalReferenceBSpline (Spline deg kts cps) x =
    sumV (zipWith (^*) (V.toList cps) (bases kts x !! deg))

-- | This is a fairly slow function which computes one polynomial segment of a B-spline (the 
-- one containing the given point), using the mathematical definition of B-splines.  It is 
-- mainly for testing purposes, as a reference against which the other evaluation functions
-- are checked.
fitPolyToBSplineAt :: (Fractional a, Ord a, Scalar a ~ a, V.Vector v a)
    => BSpline v a -> a -> Poly a
fitPolyToBSplineAt (Spline deg kts cps) x = 
    Poly.sumPolys (zipWith Poly.scalePoly (V.toList cps) (basisPolynomialsAt kts x !! deg))

ind :: Num a => Bool -> a
ind True  = 1
ind False = 0

-- | The values of all the B-spline basis functions for the given knot vector at the given
-- point, ordered by degree; \"b_{i,j}(x)\" is @bases kts x !! i !! j@.
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

-- | All the B-spline basis functions for the given knot vector at the given
-- point, ordered by degree; \"b_{i,j}\" is @basisFunctions kts x !! i !! j@.
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

-- | All the B-spline basis polynomials for the given knot vector, ordered first 
-- by knot span and then by degree.
basisPolynomials :: (Fractional a, Ord a) => Knots a -> [[[Poly a]]]
basisPolynomials kts
    | isEmpty kts   = []
    | otherwise     = [basisPolynomialsAt kts kt | kt <- init (distinctKnots kts)]

-- | All the B-spline basis polynomials for the given knot vector at the given
-- point, ordered by degree; \"b_{i,j}\" is @basisPolynomialsAt kts x !! i !! j@.
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

-- | This is a straightforward implementation of the Cox-De Boor recursion scheme
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
