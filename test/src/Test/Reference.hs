{-# LANGUAGE ParallelListComp #-}
module Test.Reference where

import Math.Spline.BSpline.Reference
import Math.Spline.Knots
import Test.QuickCheck

lerp x0 x1 a = (1-a) * x0 + a * x1

wrap  0  1 x = x - fromInteger (floor x)
wrap lo hi x = lerp lo hi (wrap 0 1 ((x - lo) / (hi - lo)))

count p = length . filter p

endKnots kts
    | null us   = Nothing
    | otherwise = Just (minimum us, maximum us)
    where us = distinctKnots kts

x `inKnotVector` kts = maybe False f (endKnots kts)
    where f (lo,hi) = lo <= x && x < hi

maxOrder x kts = p
    where
        p  = min (length gte) (length lt)
        us = knots kts
        
        (gte, lt) = break (>x) us

-- TODO: verify that this is appropriately named
fullBases kts x = take (maxOrder x kts) (bases kts x)

prop_defined     kts x = all (all (not.isNaN))      (bases kts x)
prop_nonNegative kts x = all (all (>= 0))           (bases kts x)
prop_finite      kts x = all (all (not.isInfinite)) (bases kts x)

prop_holdsEverywhere p kts = property (p kts)
prop_holdsAtKnots p kts = forAll (elements (distinctKnots kts)) (p kts)
prop_holdsOnSpans p kts = forAll (elements (spans 1 (distinctKnots kts)))
    (\(u0,u1) -> property . p kts . lerp u0 u1 =<< choose (0,1))

prop_sane kts = oneof
    [ scope test kts
    | scope <- if numKnots kts == 0
        then [prop_holdsEverywhere]
        else [prop_holdsAtKnots, prop_holdsOnSpans, prop_holdsEverywhere]
    , test  <- [prop_defined, prop_nonNegative, prop_finite]
    ]

prop_localSupport (NonNegative maxOrd) kts x = 
    not (null us) && minimum us <= x && x < maximum us ==>
    and [ and
            [ if u0 <= x && x < u1 
                then True -- don't require y /= 0, because truncation errors can make false negatives
                else y == 0
            | y <- basis
            | (u0,u1) <- spans (p+1) us
            ]
        | basis <- bases kts x
        | p <- [0..maxOrd]
        ]
    where us = knots kts

prop_cover kts x = 
    x `inKnotVector` kts ==>
    and
    [ count (/=0) basis <= max 1 (p-k+1)
    | basis <- take (p+1) (bases kts x)
    ]
    where 
        p  = maxOrder x kts
        k  = knotMultiplicity x kts

prop_partitionOfUnity eq1 kts x =
    x `inKnotVector` kts ==>
    all (eq1.sum) (fullBases kts x)

eq1 :: Rational -> Bool
eq1 = (1==)

aboutOne x = (abs (x - 1) <= 1e-14)

prop_numBases kts x = 
    m > 0 ==> and
        [ m == n + p + 1
        | basis <- bases kts x, let n = length basis
        | p <- [0..]
        ]
    where m = numKnots kts