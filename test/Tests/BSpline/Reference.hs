{-# LANGUAGE ParallelListComp, ExtendedDefaultRules, OverlappingInstances #-}
module Tests.BSpline.Reference where

import qualified Data.Vector.Safe as V
import Math.Polynomial (evalPoly, polyDegree)
import Math.Spline (BSpline, controlPoints, knotVector, splineDegree)
import Math.Spline.BSpline.Reference
import Math.Spline.BSpline.Arbitrary
import Math.Spline.Knots
import Math.Spline.Knots.Arbitrary
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

referenceBSplineTests =
    [ testGroup "bases"                 bases_tests
    , testGroup "basisFunctions"        basisFunctions_tests
    , testGroup "basisPolynomials"      basisPolynomials_tests
    , testGroup "basisPolynomialsAt"    basisPolynomialsAt_tests
    , testGroup "evalReferenceBSpline"  evalReferenceBSpline_tests
    , testGroup "fitPolyToBSplineAt"    fitPolyToBSplineAt_tests
    ]

bases_tests =
    [ testProperty "0 <= f(x) <= 1"     (directed_test prop_bases_bounded)
    , testProperty "count"              (directed_test prop_bases_count)
    , testProperty "local support"      (directed_test prop_bases_localSupport . smaller)
    , testProperty "cover"              (directed_test prop_bases_cover)
    , testProperty "partition of unity" (directed_test (prop_bases_partitionOfUnity (~=)))
    , testProperty "spot check"         prop_bases_spotCheck
    ] where x ~= y = (abs (x - y) <= 1e-14)

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

prop_bases_localSupport kts x ns = 
    and [ and
            [ if u0 < x && x < u1 
                then y /= (0::Rational)
                else y == 0 || (x == u0 && y == 1)
            | y <- basis
            | (u0,u1) <- knotSpans kts (p+1)
            ]
        | basis <- bases kts x
        | let m = numKnots kts
              cutoff = minimum (m : [n `mod` (max 1 m) | n <- ns])
            -- 'cutoff' is just an arbitrary cutoff value; checking all 
            -- functions makes the test dramatically slower than the others 
            -- here.  It was chosen by trial and error, there is nothing 
            -- sacred or meaningful about it.
        , p <- [0..cutoff] 
        ]

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

basisFunctions_tests =
    [ testProperty "equal to bases"     prop_basisFunctions_equals_bases
    ]

-- Yes, we really want EXACT equality here; these are supposed to just be
-- different interfaces to the exact same algorithm; if the results are off
-- by even one ULP, then something is wrong.
prop_basisFunctions_equals_bases = do
    kts <- resize 15 arbitrary
    x <- arbitrary
    return (bases kts x
        == [[f x | f <- basis] | basis <- basisFunctions kts])

basisPolynomials_tests =
    [ testProperty "definition"     prop_basisPolynomials_definition
    ]

-- Again, we want exact equality here; these are supposed to be different 
-- interfaces to the exact same algorithm.
prop_basisPolynomials_definition = do
    kts <- resize 10 arbitrary
    return (basisPolynomials kts
        == [basisPolynomialsAt kts kt 
           | not (isEmpty kts)
           , kt <- init (distinctKnots kts)
           ])

basisPolynomialsAt_tests =
    [ testProperty "equal to bases"     prop_basisPolynomialsAt_equals_bases
    ]

prop_basisPolynomialsAt_equals_bases = do
    kts <- resize 10 arbitrary
    x <- arbitrary
    return ((bases kts x :: [[Rational]])
        == [[evalPoly f x | f <- basis] | basis <- basisPolynomialsAt kts x])


evalReferenceBSpline_tests =
    [ testProperty "definition" prop_evalReferenceBSpline_definition
    ]

prop_evalReferenceBSpline_definition :: BSpline V.Vector Rational -> Rational -> Bool
prop_evalReferenceBSpline_definition f x
    =  evalReferenceBSpline f x
    == sum (zipWith (*) (V.toList (controlPoints f)) (bases (knotVector f) x !! splineDegree f))

fitPolyToBSplineAt_tests =
    [ testProperty "preserves degree"           prop_fitPolyToBSplineAt_degree
    , testProperty "evaluates to same value"    prop_fitPolyToBSplineAt_eval
    ]

prop_fitPolyToBSplineAt_degree :: SplineAndPoint (BSpline V.Vector) Rational -> Bool
prop_fitPolyToBSplineAt_degree (SplineAndPoint f x) =
    polyDegree (fitPolyToBSplineAt f x) <= splineDegree f

prop_fitPolyToBSplineAt_eval :: SplineAndPoint (BSpline V.Vector) Rational -> Bool
prop_fitPolyToBSplineAt_eval (SplineAndPoint f x)
    =  evalPoly (fitPolyToBSplineAt f x) x
    == evalReferenceBSpline f x