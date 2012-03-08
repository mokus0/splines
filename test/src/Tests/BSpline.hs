module Tests.BSpline where

import Control.Applicative
import Data.Maybe
import Math.Polynomial (polyDeriv)
import Math.Spline
import Math.Spline.BSpline
import Math.Spline.BSpline.Arbitrary
import Math.Spline.BSpline.Reference
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

valid f = splineDegree (bSpline kts cps) == splineDegree f
    where
        kts = knotVector f
        cps = controlPoints f

bSplineTests = 
    [ testGroup "evalBSpline"           evalBSpline_tests
    , testGroup "evalNaturalBSpline"    evalNaturalBSpline_tests
    , testGroup "differentiateBSpline"  differentiateBSpline_tests
    , testGroup "integrateBSpline"      integrateBSpline_tests
    ]

evalBSpline_tests =
    [ testProperty "agrees with evalReferenceBSpline on spline domain" prop_evalBSpline_interior
    ]

prop_evalBSpline_interior (SplineAndPoint f x) 
    = x /= x1
        ==> evalBSpline          f x 
         == evalReferenceBSpline f x
    where 
        Just (x0, x1) = splineDomain (f :: BSpline Rational)

evalNaturalBSpline_tests =
    [ testProperty "agrees with evalReferenceBSpline on spline domain" prop_evalNaturalBSpline_interior
    ]

prop_evalNaturalBSpline_interior (SplineAndPoint f x) 
    = x /= x1
        ==> evalNaturalBSpline   f x 
         == evalReferenceBSpline f x
    where 
        Just (x0, x1) = splineDomain (f :: BSpline Rational)

differentiateBSpline_tests =
    [ testProperty "agrees with polyDeriv"  prop_differentiateBSpline_definition
    , testProperty "preserves domain"       prop_differentiateBSpline_domain
    , testProperty "reduces degree by 1"    prop_differentiateBSpline_degree
    , testProperty "output valid"           prop_differentiateBSpline_valid
    ]

prop_differentiateBSpline_definition :: SplineAndPoint BSpline Rational -> Bool
prop_differentiateBSpline_definition (SplineAndPoint f x)
    =  polyDeriv (fitPolyToBSplineAt f x)
    == fitPolyToBSplineAt (differentiateBSpline f) x

prop_differentiateBSpline_domain :: NonEmptySpline BSpline Double -> Bool
prop_differentiateBSpline_domain (NonEmptySpline f)
    =  splineDomain (differentiateBSpline f)
    == splineDomain f

prop_differentiateBSpline_degree :: NonEmptySpline BSpline Double -> Bool
prop_differentiateBSpline_degree (NonEmptySpline f)
    = splineDegree (differentiateBSpline f)
    == max 0 (splineDegree f - 1)

prop_differentiateBSpline_valid :: NonEmptySpline BSpline Double -> Bool
prop_differentiateBSpline_valid (NonEmptySpline f) 
    = valid (differentiateBSpline f)

integrateBSpline_tests =
    [ testProperty "differentiateBSpline cancels"   prop_integrateBSpline_definition
    , testProperty "preserves domain"               prop_integrateBSpline_domain
    , testProperty "increases degree by 1"          prop_integrateBSpline_degree
    , testProperty "output valid"                   prop_integrateBSpline_valid
    ]

prop_integrateBSpline_definition :: SplineAndPoint BSpline Rational -> Bool
prop_integrateBSpline_definition (SplineAndPoint f x)
    =  evalBSpline (differentiateBSpline (integrateBSpline f)) x
    == evalBSpline f x

prop_integrateBSpline_domain :: BSpline Double -> Bool
prop_integrateBSpline_domain f
    =  splineDomain (integrateBSpline f)
    == splineDomain f

prop_integrateBSpline_degree :: BSpline Double -> Bool
prop_integrateBSpline_degree f
    =  splineDegree (integrateBSpline f)
    == splineDegree f + 1

prop_integrateBSpline_valid :: NonEmptySpline BSpline Double -> Bool
prop_integrateBSpline_valid (NonEmptySpline f) 
    = valid (integrateBSpline f)

