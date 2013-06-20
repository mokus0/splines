{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Math.Spline.BSpline.Arbitrary where

import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Vector.Generic as V
import Data.VectorSpace
import Math.Spline
import Math.Spline.BSpline
import Math.Spline.Knots
import Math.Spline.Knots.Arbitrary
import Test.QuickCheck

instance (Arbitrary a, Ord (Scalar a), Arbitrary (Scalar a), V.Vector v a) =>
    Arbitrary (BSpline v a) where
    arbitrary = sized $ \sz -> do
        let ktsSz = sz `div` 2
        kts <- resize (max 1 ktsSz) arbitrary `suchThat` \kts -> numKnots kts > 0
        let -- by the 'suchThat' constraint above, 1 <= m <= ∞
            m = numKnots kts - 1
            isqrt = ceiling . sqrt . fromIntegral
        d1 <- choose (0, m)
        d2 <- choose (0, m)
        let n = m - min d1 d2
        
        cps <- replicateM n arbitrary
        return $! bSpline kts (V.fromList cps)

newtype NonEmptySpline spl a = NonEmptySpline {nonEmptySpline :: spl a }
    deriving (Eq, Show)

instance (Arbitrary (spl a), Spline spl a) => Arbitrary (NonEmptySpline spl a) where
    arbitrary = NonEmptySpline <$> arbitrary `suchThat` (nonEmpty . splineDomain)
         where
             nonEmpty Nothing        = False
             nonEmpty (Just (x0,x1)) = x0 /= x1

data SplineAndPoint spl a = SplineAndPoint !(spl a) !(Scalar a)

deriving instance (Eq   (spl v), Eq   (Scalar v)) => Eq   (SplineAndPoint spl v)
deriving instance (Show (spl v), Show (Scalar v)) => Show (SplineAndPoint spl v)

instance (Arbitrary (spl v), Spline spl v, Arbitrary (Scalar v), RealFrac (Scalar v))
    => Arbitrary (SplineAndPoint spl v) where
        arbitrary = do
            NonEmptySpline f <- arbitrary
            let Just (x0, x1) = splineDomain f
            
            p <- arb01
            let clip lo hi = max lo . min hi
                x = clip x0 x1 (x0 + (x1 - x0) * p)
            
            return (SplineAndPoint f x)
