{-# LANGUAGE TypeFamilies #-}
module Math.Spline.Knots
    ( Knots
    , empty, isEmpty
    
    , knot, multipleKnot
    , mkKnots, fromList
    
    , numKnots, lookupKnot
    , toList, numDistinctKnots, lookupDistinctKnot
    
    , knots, knotsVector
    , distinctKnots, multiplicities
    , distinctKnotsVector, multiplicitiesVector
    , distinctKnotsSet
    
    , toMap
    , fromMap
    
    , toVector
    , fromVector
    
    , splitLookup
    , takeKnots, dropKnots, splitKnotsAt
    , takeDistinctKnots, dropDistinctKnots, splitDistinctKnotsAt
    
    , maxMultiplicity
    , knotMultiplicity, setKnotMultiplicity
    
    , splitFind
    
    , fromAscList, fromDistinctAscList
    , valid
    
    , knotSpan
    , knotsInSpan
    , knotSpans
    , knotDomain
    , findSpan
    
    , uniform
    
    , minKnot
    , maxKnot
    ) where

import Prelude hiding (sum, maximum)
import Control.Arrow ((***))
import Control.Monad (guard)
import Data.Foldable (Foldable(foldMap), maximum)
import Data.List (sortBy, sort, unfoldr)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.Monoid (Monoid(..))
import Data.Ord
import Data.Semigroup
import qualified Data.Set as S (Set, fromAscList)
import qualified Data.Vector as V

-- |Knot vectors - multisets of points in a 1-dimensional space.
newtype Knots a = Knots (V.Vector a) deriving (Eq, Ord)

instance Show a => Show (Knots a) where
    showsPrec p ks = showParen (p > 10)
        ( showString "mkKnots "
        . showsPrec 11 (knots ks)
        )

instance Ord a => Semigroup (Knots a) where
    (<>) = mappend

instance (Ord a) => Monoid (Knots a) where
    mempty = empty
    mappend (Knots v1) (Knots v2) =
      Knots . V.fromList . sort . V.toList $ v1 V.++ v2

instance Foldable Knots where
    foldMap f = foldMap f . knotsVector


-- |An empty knot vector
empty :: Knots a
empty = Knots V.empty

isEmpty :: Knots a -> Bool
isEmpty (Knots v) = V.null v

-- |Create a knot vector consisting of one knot.
knot :: Ord a => a -> Knots a
knot x = multipleKnot x 1

-- |Create a knot vector consisting of one knot with the specified multiplicity.
multipleKnot :: Ord a => a -> Int -> Knots a
multipleKnot k n = Knots $ V.replicate n k

-- |Create a knot vector consisting of all the knots in a list.
mkKnots :: (Ord a) => [a] -> Knots a
mkKnots = Knots . V.fromList . sort

-- |Create a knot vector consisting of all the knots and corresponding 
-- multiplicities in a list.
fromList :: (Ord k) => [(k, Int)] -> Knots k
fromList ks = Knots v
    where v = V.concat . map (\(k, mult) -> V.replicate mult k) .
              sortBy (comparing fst) $ filter ((>0).snd) ks

-- |Create a knot vector consisting of all the knots and corresponding 
-- multiplicities in a list ordered by the knots' 'Ord' instance.  The
-- ordering precondition is not checked.
fromAscList :: Eq k => [(k, Int)] -> Knots k
fromAscList ks = Knots v
    where v = V.concat . map (\(k, mult) -> V.replicate mult k)
                 $ filter ((>0).snd) ks

-- |Create a knot vector consisting of all the knots and corresponding 
-- multiplicities in a list ordered by the knots' 'Ord' instance with no
-- duplicates.  The preconditions are not checked.
fromDistinctAscList :: Eq k => [(k, Int)] -> Knots k
fromDistinctAscList = fromAscList

fromMap :: Eq k => M.Map k Int -> Knots k
fromMap = fromAscList . M.toAscList

fromVector :: Ord k => V.Vector (k,Int) -> Knots k
fromVector = fromList . V.toList

-- |Returns a list of all distinct knots in ascending order along with
-- their multiplicities.
toList :: Eq k => Knots k -> [(k, Int)]
toList = unfoldr $ \kts -> do
    kt <- minKnot kts
    return (kt, dropKnots (snd kt) kts)

toVector :: Eq k => Knots k -> V.Vector (k, Int)
toVector = V.unfoldr $ \kts -> do
    kt <- minKnot kts
    return (kt, dropKnots (snd kt) kts)

toMap :: Ord k => Knots k -> M.Map k Int
toMap = M.fromAscListWith (+) . toList

-- |Returns the number of knots (not necessarily distinct) in a knot vector.
numKnots :: Knots t -> Int
numKnots (Knots v) = V.length v

-- |Returns the number of distinct knots in a knot vector.
numDistinctKnots :: Eq t => Knots t -> Int
numDistinctKnots = V.length . distinctKnotsVector

maxMultiplicity :: Ord t => Knots t -> Int
maxMultiplicity kts@(Knots v)
    | V.length v == 0 = 0
    | otherwise = maximum $ toMap kts

lookupKnot :: Int -> Knots a -> Maybe a
lookupKnot k (Knots kts) = do
    guard (0 <= k && k < V.length kts)
    V.indexM kts k

lookupDistinctKnot :: Eq a => Int -> Knots a -> Maybe a
lookupDistinctKnot k kts = lookupKnot k . Knots $ distinctKnotsVector kts

-- |@splitLookup n kts@: Split a knot vector @kts@ into 3 parts @(pre, mbKt, post)@
-- such that:
--  
--  * All the keys in @pre@, @mbKt@ (viewed as a knot vector of either 0
-- or 1 knot), and @post@ are disjoint and ordered
--  * Putting the 3 parts back together yields exactly the original knot vector
--  * The @n@'th knot, if one exists, will be in @mbKt@ along with its multiplicity
--
splitLookup :: Int -> Knots a -> (Knots a, Maybe a, Knots a)
splitLookup k (Knots v)
    | V.null gt = (Knots lt, Nothing, Knots V.empty)
    | otherwise = (Knots lt, Just $ V.head gt, Knots $ V.tail gt)
  where
    (lt, gt) = V.splitAt k v

dropKnots :: Int -> Knots a -> Knots a
dropKnots k (Knots v) = Knots $ V.drop k v

takeKnots :: Int -> Knots a -> Knots a
takeKnots k (Knots v) = Knots $ V.take k v

splitKnotsAt :: Int -> Knots a -> (Knots a, Knots a)
splitKnotsAt k (Knots v) = Knots *** Knots $ V.splitAt k v

-- |Count the number of knots less than the n'th distinct knot.
findDistinctKnot :: Eq a => Int -> Knots a -> Int
findDistinctKnot n = V.last . V.take (1 + max 0 n) . V.scanl (+) 0 . multiplicitiesVector

takeDistinctKnots :: (Ord a) => Int -> Knots a -> Knots a
takeDistinctKnots k kts = takeKnots (findDistinctKnot k kts) kts

dropDistinctKnots :: (Ord a) => Int -> Knots a -> Knots a
dropDistinctKnots k kts = dropKnots (findDistinctKnot k kts) kts

splitDistinctKnotsAt :: (Ord a, Eq a) => Int -> Knots a -> (Knots a, Knots a)
splitDistinctKnotsAt k kts = splitKnotsAt (findDistinctKnot k kts) kts

-- |Returns a list of all knots (not necessarily distinct) of a knot vector in ascending order
knots :: Knots t -> [t]
knots = V.toList . knotsVector

-- |Returns a vector of all knots (not necessarily distinct) of a knot vector in ascending order
knotsVector :: Knots t -> V.Vector t
knotsVector (Knots v) = v

-- |Returns a list of all distinct knots of a knot vector in ascending order
distinctKnots :: Eq t => Knots t -> [t]
distinctKnots = map fst . toList

multiplicities :: Eq t => Knots t -> [Int]
multiplicities = map snd . toList

-- |Returns a vector of all distinct knots of a knot vector in ascending order
distinctKnotsVector :: Eq t => Knots t -> V.Vector t
distinctKnotsVector = V.map fst . toVector

multiplicitiesVector :: Eq a => Knots a -> V.Vector Int
multiplicitiesVector = V.map snd . toVector

-- |Returns a 'S.Set' of all distinct knots of a knot vector
distinctKnotsSet :: Eq k => Knots k -> S.Set k
distinctKnotsSet (Knots k) = S.fromAscList $ V.toList k

-- |Looks up the multiplicity of a knot (which is 0 if the point is not a knot)
knotMultiplicity :: (Ord k) => k -> Knots k -> Int
knotMultiplicity k (Knots ks) = V.length $ V.elemIndices k ks

-- |Returns a new knot vector with the given knot set to the specified 
-- multiplicity and all other knots unchanged.
setKnotMultiplicity :: Ord k => k -> Int -> Knots k -> Knots k
setKnotMultiplicity k n kts@(Knots v)
    | n <= 0    = Knots (V.filter (/= k) v)
    | otherwise = Knots $ V.concat [lt, V.replicate n k, gt]
        where (Knots lt, _, Knots gt) = splitFind k kts

splitFind :: Ord k => k -> Knots k -> (Knots k, Knots k, Knots k)
splitFind k (Knots v) = (Knots lt, Knots eq, Knots gt)
  where
    (lt, xs) = V.span (<k) v
    (eq, gt) = V.span (==k) xs

-- |Check the internal consistency of a knot vector
valid :: (Ord k, Num k) => Knots k -> Bool
valid (Knots v)
    | V.null v  = True
    | otherwise = V.and $ V.zipWith (>=) (V.tail v) v

-- |@knotSpan kts i j@ returns the knot span extending from the @i@'th knot
-- to the @j@'th knot, if  @i <= j@ and both knots exist.
knotSpan :: Knots a -> Int -> Int -> Maybe (a, a)
knotSpan ks i j = do
    guard (i <= j)
    lo <- lookupKnot i ks
    hi <- lookupKnot j ks
    return (lo,hi)

-- |@knotsInSpan kts i j@ returns the knots in the knot span extending from
-- the @i@'th knot to the @j@'th knot
knotsInSpan :: Knots a -> Int -> Int -> Knots a
knotsInSpan kts i j = takeKnots (j - i) (dropKnots i kts)

-- |@knotSpans kts width@ returns all knot spans of a given width in
-- ascending order.
--
-- For example, @knotSpans (mkKnots [1..5]) 2@ yields @[(1,3), (2,4), (3,5)]@.
knotSpans :: Knots a -> Int -> [(a,a)]
knotSpans ks w
    | w <= 0    = error "knotSpans: width must be positive"
    | otherwise = zip kts (drop w kts)
    where kts = knots ks

-- | @findSpan kv p u@ returns i such that u falls between the @i@'th knot
-- and knot @i+1@.  In particular, for multiple knots, the highest i is chosen.
-- If u is equal to the highest knot value, the highest knot with lower parameter
-- is returned.
findSpan :: Ord a => Knots a -> a -> Maybe Int
findSpan k u =
  case minKnot k of
    Nothing -> Nothing
    Just (lo, _) | lo > u    -> Nothing
    _ -> case maxKnot k of
      Nothing                -> Nothing
      Just (hi, m) | hi == u -> Just $ (numKnots k) - m - 1
                   | hi <  u -> Nothing
      _                      -> Just $ mult - 1 where
        distinct = V.findIndex (> u) (distinctKnotsVector k)
        mult = V.sum $ V.take (fromJust distinct) $ multiplicitiesVector k

-- |@knotDomain kts p@ returns the domain of a B-spline or NURBS with knot
-- vector @kts@ and degree @p@.  This is the subrange spanned by all
-- except the first and last @p@ knots.  Outside this domain, the spline
-- does not have a complete basis set.  De Boor's algorithm assumes that
-- the basis functions sum to 1, which is only true on this range, and so
-- this is also precisely the domain on which de Boor's algorithm is valid.
knotDomain :: Knots a -> Int -> Maybe (a,a)
knotDomain ks@(Knots v) p = knotSpan ks p (V.length v-p-1)

-- |@uniform deg nPts (lo,hi)@ constructs a uniformly-spaced knot vector over
-- the interval from @lo@ to @hi@ which, when used to construct a B-spline
-- with @nPts@ control points will yield a clamped spline with degree @deg@.
uniform :: (Ord s, Fractional s) => Int -> Int -> (s,s) -> Knots s
uniform deg nPts (lo,hi) = ends `mappend` internal
    where
        ends = fromList [(lo,deg), (hi,deg)]
        n = nPts + deg - numKnots ends
        f i = (fromIntegral i * lo + fromIntegral (n - i) * hi) / fromIntegral n
        internal = mkKnots [f i | i <- [0..n]]

{-# INLINE minKnot #-}
minKnot :: (Eq a) => Knots a -> Maybe (a, Int)
minKnot (Knots v)
    | V.null v  = Nothing
    | otherwise = Just (kt, V.length (V.takeWhile (kt ==) v))
    where kt = V.head v

{-# INLINE maxKnot #-}
maxKnot :: Eq a => Knots a -> Maybe (a, Int)
maxKnot (Knots v) = minKnot (Knots (V.reverse v))
