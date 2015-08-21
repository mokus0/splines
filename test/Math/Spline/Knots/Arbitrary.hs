{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Math.Spline.Knots.Arbitrary
    ( Multiplicity(..), multiplicity
    , Smaller(..), smaller
    , KnotList(..)
    , KnotMap(..)
    , directed_test
    , arb01  -- TODO: move this to a general-purpose "support" module...
             -- probably also specialize it for choosing from a specified range
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
        nKts <- choose (0, max 1 sz)
        let maxM = max 1 ((sz + nKts - 1) `div` nKts)
        kts <- replicateM nKts $ do
            x <- arbitrary
            m <- choose (1, maxM)
            return (x,m)
            
        return (fromList kts)

newtype Smaller a = Smaller a deriving (Eq, Ord, Enum, Num, Show)

smaller (Smaller x) = x

instance Arbitrary a => Arbitrary (Smaller a) where
    arbitrary = sized $ \sz -> do
        x <- resize ((sz * 2) `div` 3) arbitrary
        return (Smaller x)

-- For many tests, we want to make sure certain classes of inputs are
-- tested.  In particular, the knot values themselves should be tested with 
-- some regularity and more testing should be done inside the knot vector than
-- outside.  So, we use this test-driver to focus the tests where we want them.
-- The weights are more or less arbitrary.
-- 
-- The @take (1 + numDistinctKnots kts)@ part is just to limit the cases 
-- considered to those that are possible for the given knot vector
directed_test test kts = frequency $ take (1 + numDistinctKnots kts)
        [ (1, return everywhere)
        , (1, return atKnots)
        , (5, return onSpans)
        ]
    where
        everywhere = property (test kts)
        atKnots = forAll (elements (distinctKnots kts)) (test kts)
        onSpans = forAll (elements (spans (distinctKnots kts)))
            (\(u0,u1) -> forAll arb01 (property . test kts . lerp u0 u1))
        
        spans xs = zip xs (tail xs)
        lerp x0 x1 a = (1-a) * x0 + a * x1

-- Pick an arbitrary value x in the range 0 <= x <= 1
-- I was using @choose (0,1)@ for this, but that didn't work for 
-- Rational because it isn't an instance of System.Random.Random.
-- Since I don't really care about the quality of the actual 
-- distribution, this is a good-enough substitute that uses Arbitrary
-- instead of Random.
arb01 :: (Arbitrary a, RealFrac a) => Gen a
arb01 = do
    x <- arbitrary;
    let xI    = floor x
        iPart = fromInteger xI
    return $! if iPart == x
        then if even xI then 0 else 1
        else x - iPart

