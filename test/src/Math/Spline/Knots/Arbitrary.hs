{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Math.Spline.Knots.Arbitrary
    ( Multiplicity(..), multiplicity
    , Smaller(..), smaller
    , KnotList(..)
    , KnotMap(..)
    ) where

import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Monoid
import Math.Spline.Knots
import Test.QuickCheck

newtype Multiplicity = Multiplicity Int deriving (Eq, Ord, Enum, Num, Show)
multiplicity (Multiplicity m) = m

instance Arbitrary Multiplicity where
    arbitrary = sized $ \sz -> do
        m <- choose (-sz, sz)
        return (Multiplicity m)

instance CoArbitrary Multiplicity where
    coarbitrary (Multiplicity n) = coarbitrary n

newtype KnotList a = KnotList [(a, Int)] deriving (Eq, Ord, Show)
instance Arbitrary a => Arbitrary (KnotList a) where
    arbitrary = do
        kts <- arbitrary
        return (KnotList [(k,m) | (k, Multiplicity m) <- kts])

instance CoArbitrary a => CoArbitrary (KnotList a) where
    coarbitrary (KnotList kts) = coarbitrary [(k, Multiplicity m) | (k,m) <- kts]

newtype KnotMap a = KnotMap (M.Map a Int) deriving (Eq, Ord, Show)
instance (Arbitrary a, Ord a) => Arbitrary (KnotMap a) where
    arbitrary = do
        KnotList kts <- arbitrary
        return (KnotMap (M.fromList kts))

instance (Arbitrary a, Ord a) => Arbitrary (Knots a) where
    arbitrary = sized $ \sz -> do
        nKts <- choose (0,sz)
        let maxM = sz `div` nKts
        kts <- replicateM nKts $ do
            x <- arbitrary
            m <- choose (1, maxM)
            return (x,m)
            
        return (fromList kts)

newtype Smaller a = Smaller a deriving (Eq, Ord, Enum, Num, Show)

smaller (Smaller x) = x

instance Arbitrary a => Arbitrary (Smaller a) where
    arbitrary = sized $ \sz -> do
        x <- resize (sz `div` 2) arbitrary
        return (Smaller x)
