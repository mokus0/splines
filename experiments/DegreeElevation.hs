{-# LANGUAGE ParallelListComp #-}
module Math.Spline.BSpline.DegreeElevation where

import Math.Spline.BSpline
import Math.Spline.Class
import Math.Spline.Knots

import Control.Monad (guard)
import Data.List (tails, zipWith5)
import Data.VectorSpace

clampLeft spline = do
    (t, _) <- splineDomain spline
    (_, clamped) <- splitBSpline spline t
    return clamped

clampRight spline = do
    (_, t) <- splineDomain spline
    (clamped, _) <- splitBSpline spline t
    return clamped

clamp spline = do
    (t0, t1) <- splineDomain spline
    (_, lClamped) <- splitBSpline spline t0
    (clamped,  _) <- splitBSpline lClamped t1
    return clamped

spans n xs = zip xs (drop n xs)
spansWith op n xs = zipWith op xs (drop n xs)

modifiedDifCoeffs spline = take p $ go (drop 1 ts) (map Just (controlPoints spline))
    where
        p = splineDegree spline + 1
        ts = knots (knotVector spline)
        tHi = drop p ts
        go tLo [] = []
        go tLo ps = ps : go (drop 1 tLo) 
            [ do
                p0 <- p0
                p1 <- p1
                guard (t1 /= t0)
                return (recip (t1 - t0) *^ (p1 ^-^ p0) )
            | p0:p1:_ <- tails ps 
            | t0 <- tLo 
            | t1 <- tHi
            ]

condense kts  = go (interior (distinctKnots kts))
    where
        interior = reverse . drop 1 . reverse . drop 1
        
        go    []  xs   = take 1 xs
        go (u:us) xs   = head xs : go us (drop (knotMultiplicity u kts) xs)

-- inputs: knots and modifiedDifCoeffs output format, reversed, with known values lifted
-- by 'Just' and unknown or uninteresting values indicated by 'Nothing'
reconstruct ts pss = go (k-1) (repeat (Just zeroV)) pss
    where
        k = length pss - 1
        tHi = drop k ts
        go j prev ~(ps: pss)
            | j < 0     = [new]
            | otherwise = new : go (j-1) new pss
            where 
                tLo = drop (j+1) ts
                new = zipWith5 recon ps (Nothing:new) (Nothing : prev) tLo tHi
        
        recon (Just x) _ _ _ _ = Just x
        recon Nothing p0 p1 t0 t1 = do
            p0 <- p0
            p1 <- p1
            return (p0 ^+^ ((t1 - t0) *^ p1))

