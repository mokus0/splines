{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Math.Spline.Knots.Arbitrary where

import Control.Monad
import qualified Data.Map as M
import Data.Monoid
import Math.Spline.Knots
import Test.QuickCheck

newtype Multiplicity = Multiplicity Int deriving (Eq, Ord, Enum, Num, Show)
multiplicity (Multiplicity m) = m

minMultiplicity = -1000
maxMultiplicity =  1000
instance Arbitrary Multiplicity where
    arbitrary = sized $ \sz -> do
        let fsz = sz + 4
        m <- choose (max (-fsz) minMultiplicity, min fsz maxMultiplicity)
        return (Multiplicity m)

instance CoArbitrary Multiplicity where
    coarbitrary (Multiplicity n) = coarbitrary n

newtype KnotList a = KnotList [(a, Int)] deriving (Eq, Ord, Show)
instance Arbitrary a => Arbitrary (KnotList a) where
    arbitrary = do
        kts <- arbitrary
        return (KnotList [(k,m) | (k, Multiplicity m) <- kts])

newtype KnotMap a = KnotMap (M.Map a Int) deriving (Eq, Ord, Show)
instance (Arbitrary a, Ord a) => Arbitrary (KnotMap a) where
    arbitrary = do
        KnotList kts <- arbitrary
        return (KnotMap (M.fromList kts))

instance (Arbitrary a, Ord a) => Arbitrary (Knots a) where
    arbitrary = do
        KnotList kts <- arbitrary
        return (fromList kts)
