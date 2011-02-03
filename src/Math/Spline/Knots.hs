{-# LANGUAGE TypeFamilies #-}
module Math.Spline.Knots
    ( Knots
    , empty, isEmpty
    
    , knot, multipleKnot
    , mkKnots, fromList
    
    , knots, numKnots, lookupKnot
    , toList, distinctKnots, numDistinctKnots, lookupDistinctKnot
    
    , toMap
    , fromMap
    
    , splitLookup
    , takeKnots, dropKnots, splitKnotsAt
    , takeDistinctKnots, dropDistinctKnots, splitDistinctKnotsAt
    
    , knotMultiplicity, setKnotMultiplicity
    
    , fromAscList, fromDistinctAscList
    , valid
    
    , knotSpan
    , knotSpans
    , knotDomain
    
    , interior
    
    , uniform
    ) where

import Prelude hiding (sum)
import Control.Monad (guard)
import Data.Foldable (Foldable(foldMap), sum)
import qualified Data.Map as M
import Data.Monoid (Monoid(..))
import Data.Maybe (fromMaybe)
import qualified Data.Set as S (Set)
import Data.VectorSpace

-- |Knot vectors - multisets of points in a 1-dimensional space.
data Knots a = Knots !Int (M.Map a Int) deriving (Eq, Ord)

instance Show a => Show (Knots a) where
    showsPrec p ks@(Knots 0 _) = showString "empty"
    showsPrec p ks@(Knots 1 _) = showParen (p > 10)
        ( showString "knot "
        . showsPrec 11 (head $ knots ks)
        )
    showsPrec p ks = showParen (p > 10)
        ( showString "mkKnots "
        . showsPrec 11 (knots ks)
        )

instance (Ord a) => Monoid (Knots a) where
    mempty = empty
    mappend (Knots n1 v1) (Knots n2 v2) =
        Knots (n1 + n2) (M.filter (/=0) (M.unionWith (+) v1 v2))

instance Foldable Knots where
    foldMap f = foldMap f . knots


-- |An empty knot vector
empty :: Knots a
empty = Knots 0 M.empty

isEmpty :: Knots a -> Bool
isEmpty (Knots 0 _) = True
isEmpty  _          = False

-- |Create a knot vector consisting of one knot.
knot :: Ord a => a -> Knots a
knot x = multipleKnot x 1

-- |Create a knot vector consisting of one knot with the specified multiplicity.
multipleKnot :: Ord a => a -> Int -> Knots a
multipleKnot k n 
    | n <= 0    = Knots 0 (M.empty)
    | otherwise = Knots n (M.singleton k n)

-- |Create a knot vector consisting of all the knots in a list.
mkKnots :: (Ord a) => [a] -> Knots a
mkKnots ks = fromList (map (\k -> (k,1)) ks)

-- |Create a knot vector consisting of all the knots and corresponding 
-- multiplicities in a list.
fromList :: (Ord k) => [(k, Int)] -> Knots k
fromList ks = Knots (sum kMap) kMap
    where kMap = M.fromListWith (+) (filter ((>0).snd) ks)

-- |Create a knot vector consisting of all the knots and corresponding 
-- multiplicities in a list ordered by the knots' 'Ord' instance.  The
-- ordering precondition is not checked.
fromAscList :: Eq k => [(k, Int)] -> Knots k
fromAscList ks = Knots (sum kMap) kMap
    where kMap = M.fromAscListWith (+) (filter ((>0).snd) ks)

-- |Create a knot vector consisting of all the knots and corresponding 
-- multiplicities in a list ordered by the knots' 'Ord' instance with no
-- duplicates.  The preconditions are not checked.
fromDistinctAscList :: [(k, Int)] -> Knots k
fromDistinctAscList ks = Knots (sum kMap) kMap
    where kMap = M.fromDistinctAscList (filter ((>0).snd) ks)

fromMap :: M.Map k Int -> Knots k
fromMap ks = Knots (sum kMap) kMap
    where
        kMap = mFilter (>0) ks
        -- filter is monotonic, I have no idea why M.filter requires Ord on the key
        mFilter p = M.fromDistinctAscList . filter (p.snd) . M.toAscList

-- |Returns a list of all distinct knots in ascending order along with
-- their multiplicities.
toList :: Knots k -> [(k, Int)]
toList = M.toList . toMap

toMap :: Knots k -> M.Map k Int
toMap (Knots _ ks) = ks

-- |Returns the number of knots (not necessarily distinct) in a knot vector.
numKnots :: Knots t -> Int
numKnots (Knots n _) = n

-- |Returns the number of distinct knots in a knot vector.
numDistinctKnots :: Knots t -> Int
numDistinctKnots (Knots _ ks) = M.size ks

lookupKnot :: Int -> Knots a -> Maybe a
lookupKnot k kts
    | k < 0             = Nothing
    | k < numKnots kts  = fmap fst mbKt
    | otherwise         = Nothing
    where (_, mbKt, _) = splitLookup k kts

lookupDistinctKnot :: Int -> Knots a -> Maybe a
lookupDistinctKnot k (Knots _ ks)
    | k < 0         = Nothing
    | k < M.size ks = Just (fst (M.elemAt k ks))
    | otherwise     = Nothing

-- |@splitLookup n kts@: Split a knot vector @kts@ into 3 parts @(pre, mbKt, post)@
-- such that:
--  
--  * All the keys in @pre@, @mbKt@ (viewed as a knot vector of either 0
-- or 1 knot), and @post@ are disjoint and ordered
--  * Putting the 3 parts back together yields exactly the original knot vector
--  * The @n@'th knot, if one exists, will be in @mbKt@ along with its multiplicity
--
splitLookup :: Int -> Knots a -> (Knots a, Maybe (a, Int), Knots a)
splitLookup k (Knots n ks) = scan 0 M.empty n ks
    where
        -- The general plan: iteratively pull the smallest knot out of "post",
        -- either moving it to "pre" or terminating by returning it along with
        -- current values of "pre" and "post"
        
        -- invariants:
        --   nPre  = sum pre
        --   nPost = sum post
        --   M.union pre post = ks
        --   every key in pre < every key in post
        scan nPre pre nPost post
            | nPost <= 0    = (Knots nPre  pre, Nothing, Knots nPost post)
            | nPre + m > k  = (Knots nPre  pre, Just kt, Knots nNewPost newPost)
            | otherwise     = scan (nPre + m) (pre `ascSnoc` kt) nNewPost newPost
            where
                Just (kt@(x,m), newPost)  = M.minViewWithKey post
                nNewPost = nPost - m
                done x = (Knots nPre  pre, x, Knots nPost post)

-- Prepend or append an element to a map, without checking the precondition
-- that the new pair's key is less than (greater than, resp.) all keys in 
-- the map.
ascCons x m = M.fromDistinctAscList (x : M.toAscList m)
ascSnoc m x = M.fromDistinctAscList (M.toAscList m ++ [x])

-- Prepend or append an knot to a knot vector, without checking the
-- precondition that the new knot's location is less than (greater than,
-- resp.) all knots in the vector.
ascConsKnot (_,0) kts = kts
ascConsKnot kt@(k,m) (Knots n ks) = Knots (n+m) (kt `ascCons` ks)

ascSnocKnot kts (_,0) = kts
ascSnocKnot (Knots n ks) kt@(k,m) = Knots (n+m) (ks `ascSnoc` kt)

clamp lo hi = max lo . min hi

dropKnots :: Int -> Knots a -> Knots a
dropKnots k kts = fromMaybe post $ do
        (x,xAvail) <- mbKt
        let xWanted = numKnots kts - (numKnots post + k)
        
        return ((x, clamp 0 xAvail xWanted) `ascConsKnot` post)
    where
        (pre, mbKt, post) = splitLookup k kts

takeKnots :: Int -> Knots a -> Knots a
takeKnots k kts = fromMaybe pre $ do
        (x,xAvail) <- mbKt
        let xWanted = k - numKnots pre
    
        return (pre `ascSnocKnot` (x, clamp 0 xAvail xWanted))
    where
        (pre, mbKt, post) = splitLookup k kts

splitKnotsAt :: Int -> Knots a -> (Knots a, Knots a)
splitKnotsAt k kts = fromMaybe (pre, post) $ do
        (x,xAvail) <- mbKt
        let xWanted = k - numKnots pre
            xTaken = clamp 0 xAvail xWanted
    
        return ( pre `ascSnocKnot` (x,xTaken)
               , (x, xAvail - xTaken) `ascConsKnot` post
               )
    where
        (pre, mbKt, post) = splitLookup k kts


takeDistinctKnots :: Int -> Knots a -> Knots a
takeDistinctKnots k (Knots n ks) = Knots (sum kMap) kMap
    where
        kMap = M.fromDistinctAscList (take k (M.toAscList ks))

dropDistinctKnots :: Int -> Knots a -> Knots a
dropDistinctKnots k (Knots n ks) = Knots (sum kMap) kMap
    where
        kMap = M.fromDistinctAscList (drop k (M.toAscList ks))

splitDistinctKnotsAt :: Int -> Knots a -> (Knots a, Knots a)
splitDistinctKnotsAt k (Knots n ks) = (Knots sz1 kMap1, Knots (n - sz1) kMap2)
    where
        (ks1, ks2) = splitAt k (M.toAscList ks)
        kMap1 = M.fromDistinctAscList ks1
        kMap2 = M.fromDistinctAscList ks2
        sz1   = sum kMap1

-- |Returns a list of all knots (not necessarily distinct) of a knot vector in ascending order
knots :: Knots t -> [t]
knots (Knots _ ks) = concat [replicate n k | (k,n) <- M.toAscList ks]

-- |Returns a list of all distinct knots of a knot vector in ascending order
distinctKnots :: Knots t -> [t]
distinctKnots (Knots _ ks) = M.keys ks

-- |Returns a 'S.Set' of all distinct knots of a knot vector
distinctKnotsSet :: Knots k -> S.Set k
distinctKnotsSet (Knots _ ks) = M.keysSet ks

-- |Looks up the multiplicity of a knot (which is 0 if the point is not a knot)
knotMultiplicity :: (Ord k) => k -> Knots k -> Int
knotMultiplicity k (Knots _ ks) = fromMaybe 0 (M.lookup k ks)

-- |Returns a new knot vector with the given knot set to the specified 
-- multiplicity and all other knots unchanged.
setKnotMultiplicity :: Ord k => k -> Int -> Knots k -> Knots k
setKnotMultiplicity k n (Knots m ks)
    | n <= 0    = Knots (m     - n') (M.delete k ks)
    | otherwise = Knots (m + n - n') (M.insert k n ks)
    where
        n' = knotMultiplicity k (Knots m ks)

-- |Check the internal consistency of a knot vector
valid :: Ord k => Knots k -> Bool
valid (Knots n ks) = and
    [ M.valid ks
    , n == sum ks
    , all ((>0).snd) (M.toAscList ks)
    ]

-- |@knotSpan kts i j@ returns the knot span extending from the @i@'th knot
-- to the @j@'th knot, if  @i <= j@ and both knots exist.
knotSpan :: Knots a -> Int -> Int -> Maybe (a, a)
knotSpan ks i j = do
    guard (i <= j)
    lo <- lookupKnot i ks
    hi <- lookupKnot j ks
    return (lo,hi)

-- |@knotSpans kts width@ returns all knot spans of a given width in
-- ascending order.
--
-- For example, @knotSpans (mkKnots [1..5]) 2@ yields @[(1,3), (2,4), (3,5)]@.
knotSpans :: Knots a -> Int -> [(a,a)]
knotSpans ks w
    | w <= 0    = error "knotSpans: width must be positive"
    | otherwise = zip kts (drop w kts)
    where kts = knots ks

-- |@knotDomain kts p@ returns the domain of a B-spline or NURBS with knot
-- vector @kts@ and degree @p@.  This is the subrange spanned by all
-- except the first and last @p@ knots.  Outside this domain, the spline
-- does not have a complete basis set.  De Boor's algorithm assumes that
-- the basis functions sum to 1, which is only true on this range, and so
-- this is also precisely the domain on which de Boor's algorithm is valid.
knotDomain :: Knots a -> Int -> Maybe (a,a)
knotDomain ks@(Knots n _) p = knotSpan ks p (n-p-1)

-- |Drop the first and last distinct knots from a knot vector, yielding a
-- vector of interior knots
interior :: Knots a -> Knots a
interior kts = dropDistinctKnots 1 . takeDistinctKnots (numDistinctKnots kts - 1) $ kts

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
