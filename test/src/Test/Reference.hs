{-# LANGUAGE ParallelListComp, ExtendedDefaultRules #-}
module Test.Reference where

import Math.Spline.BSpline.Reference
import Math.Spline.Knots
import Math.Spline.Knots.Arbitrary
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

referenceBSplineTests =
    [ testGroup "bases" bases_tests
    ]

bases_tests =
    [ testProperty "0 <= f(x) <= 1"     (directed_test prop_bases_bounded)
    , testProperty "count"              (directed_test prop_bases_count)
    , testProperty "local support"      (directed_test prop_bases_localSupport)
    , testProperty "cover"              (directed_test prop_bases_cover)
    , testProperty "partition of unity" (directed_test (prop_bases_partitionOfUnity (~=)))
    , testProperty "spot check"         prop_bases_spotCheck
    ] where x ~= y = (abs (x - y) <= 1e-14)


-- For all of these tests, we want to make sure certain classes of inputs are
-- tested.  In particular, the knot values themselves should be tested with 
-- some regularity and more testing should be done inside the knot vector than
-- outside.  So, we use this test-driver to focus the tests where we want them.
-- The weights are more or less arbitrary.
-- 
-- The @take (1 + numDistinctKnots kts)@ part is just to limit the cases 
-- considered to those that are possible for the given knot vector
directed_test test kts = frequency $ take (1 + numDistinctKnots kts)
        [ (1, everywhere)
        , (1, atKnots)
        , (5, onSpans)
        ]
    where
        everywhere = property (test kts)
        atKnots = forAll (elements (distinctKnots kts)) (test kts)
        onSpans = forAll (elements (spans 1 (distinctKnots kts)))
            (\(u0,u1) -> forAll arb01 (property . test kts . lerp u0 u1))
        
        lerp x0 x1 a = (1-a) * x0 + a * x1
        
        -- I was using @choose (0,1)@ for this, but that didn't work for 
        -- Rational because it isn't an instance of System.Random.Random.
        -- Since I don't really care about the quality of the actual 
        -- distribution, this is a good-enough substitute that uses Arbitrary
        -- instead of Random.
        arb01 = do x <- arbitrary; return (x - fromInteger (floor x))

prop_bases_bounded kts x =
    not (null bs) ==> forAll (elements bs) (all inBounds)
    where 
        bs = filter (not.null) (bases kts x)
        inBounds y = (0 <= y && y <= 1)

prop_bases_count kts x = 
    m > 0 ==> and
        [ m == n + p + 1
        | basis <- bases kts x, let n = length basis
        | p <- [0..]
        ]
    where m = numKnots kts

prop_bases_localSupport kts x (NonNegative maxOrd) = 
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

-- For every point in the knot vector, for every basis, there are no more than
-- p-k+1 nonzero basis functions (where p is the order of the basis and k is 
-- the multiplicity of the input point in the knot vector).
--
-- Not using an equality-based check because it would give false positives in
-- many cases where function values are extremely small due to very high order
-- splines.
prop_bases_cover kts x = and
    [ count (/=0) basis <= max 0 (p-k) + 1
    | basis <- bases kts x
    | p <- [0..]
    ]
    where 
        k  = knotMultiplicity x kts

count p = length . filter p

prop_bases_partitionOfUnity (==) kts x =
    not (null fullBases) ==>
        all ((1==).sum) fullBases
    where
        -- The set of bases which are \"full\" at @x@: They sum to 1 and form a complete
        -- basis for polynomials of their degree.
        fullBases = take numFullBases (bases kts x)
        
        -- The number of bases for which the partition of unity property
        -- should hold at a given point.  The closer the point is to the
        -- edges of the knot vector, the lower this will be.
        numFullBases = min (length gte) (length lt)
            where
                (gte, lt) = break (>x) (knots kts)

-- based on an example given at:
-- http://www.cs.mtu.edu/~shene/COURSES/cs3621/NOTES/spline/B-spline/bspline-ex-1.html
prop_bases_spotCheck = directed_test expected spotCheck_kts
    where
        expected kts u = take 3 (bases kts u) == spotCheck_expected u

spotCheck_kts :: Knots Rational
spotCheck_kts = mkKnots [0,0,0,0.3,0.5,0.5,0.6,1,1,1]
spotCheck_expected u =
    [ [ 0
      , 0
      , 1 `on` (0.0,0.3)
      , 1 `on` (0.3,0.5)
      , 0
      , 1 `on` (0.5,0.6)
      , 1 `on` (0.6,1.0)
      , 0
      , 0
      ]
    , [ 0
      , 1 - (10/3)*u        `on` (0.0,0.3)
      , maximum
        [ (10/3)*u          `on` (0.0,0.3)
        , 2.5*(1 - 2*u)     `on` (0.3,0.5)
        ]
      , (5*u - 1.5)         `on` (0.3,0.5)
      , 6 - 10*u            `on` (0.5,0.6)
      , maximum
        [ 10*u - 5          `on` (0.5,0.6)
        , 2.5*(1 - u)       `on` (0.6,1.0)
        ]
      , 2.5*u - 1.5         `on` (0.6,1.0)
      , 0
      ]
    , [ (1 - (10/3)*u)^2                `on` (0.0,0.3)
      , maximum
        [ (20/3)*(u - (8/3)*u^2)        `on` (0.0,0.3)
        , 2.5*(1 - 2*u)^2               `on` (0.3,0.5)
        ]
      , maximum
        [ (20/3)*u^2                    `on` (0.0,0.3)
        , (-3.75) + 25*u - 35*u^2       `on` (0.3,0.5)
        ]
      , maximum
        [ (5*u - 1.5)^2                 `on` (0.3,0.5)
        , (6 - 10*u)^2                  `on` (0.5,0.6)
        ]
      , maximum
        [ 20*((-2) + 7*u - 6*u^2)       `on` (0.5,0.6)
        , 5*(1 - u)^2                   `on` (0.6,1.0)
        ]
    {- These last 3 were wrong on the web page; recalculated by hand -}
      , maximum
        [ (2*u - 1)*(10*u - 5)          `on` (0.5,0.6)
        , 2.5*(1 - u)*(4.5*u - 2.5)     `on` (0.6,1.0)
        ]
      , (2.5*u - 1.5)^2                 `on` (0.6,1.0)
      ]
    ] 
    where
        infix 0 `on`
        on fx (x0, x1) 
            | u < x0    = 0
            | u >= x1   = 0
            | otherwise = fx
