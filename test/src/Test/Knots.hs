{-# LANGUAGE ExtendedDefaultRules #-}
module Test.Knots where

import Control.Arrow
import Control.Monad (guard)
import Data.Ord
import Data.Function (on)
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Math.Spline.Knots
import Math.Spline.Knots.Arbitrary
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

default (Integer, Double)

knotsTests = 
    [ testGroup "Constructors and Destructors" 
        [ testGroup "empty"                 empty_tests
        , testGroup "knot"                  knot_tests
        , testGroup "multipleKnot"          multipleKnot_tests
        , testGroup "mkKnots"               mkKnots_tests
        , testGroup "fromList"              fromList_tests
        , testGroup "fromMap"               fromMap_tests
        , testGroup "fromAscList"           fromAscList_tests
        , testGroup "fromDistinctAscList"   fromDistinctAscList_tests
        ]
    , testGroup "Knot vector manipulations"
        [ testGroup "splitLookup"           splitLookup_tests
        , testGroup "takeKnots"             takeKnots_tests
        , testGroup "dropKnots"             dropKnots_tests
        , testGroup "splitKnotsAt"          splitKnotsAt_tests
        , testGroup "takeDistinctKnots"     takeDistinctKnots_tests
        , testGroup "dropDistinctKnots"     dropDistinctKnots_tests
        , testGroup "splitDistinctKnotsAt"  splitDistinctKnotsAt_tests
        , testGroup "setKnotMultiplicity"   setKnotMultiplicity_tests
        ]
    , testGroup "Lookups"
        [ testGroup "lookupKnot"            lookupKnot_tests
        , testGroup "lookupDistinctKnot"    lookupDistinctKnot_tests
        ]
    ]

-- * Constructors

-- empty
empty_tests =
    [ testProperty "round-trip via knots"           prop_empty_knots_roundTrip
    , testProperty "round-trip via distinctKnots"   prop_empty_distinctKnots_roundTrip
    , testProperty "round-trip via toList"          prop_empty_toList_roundTrip
    , testProperty "round-trip via toMap"           prop_empty_toMap_roundTrip
    , testProperty "isEmpty"                        prop_empty_isEmpty
    , testProperty "numKnots"                       prop_empty_numKnots
    , testProperty "numDistinctKnots"               prop_empty_numDistinctKnots
    , testProperty "knotMultiplicity"               prop_empty_knotMultiplicity
    , testProperty "valid"                          prop_empty_valid
    ]

prop_empty_knots_roundTrip         = knots              empty == []
prop_empty_distinctKnots_roundTrip = distinctKnots      empty == []
prop_empty_toList_roundTrip        = toList             empty == []
prop_empty_toMap_roundTrip         = toMap              empty == M.empty
prop_empty_isEmpty                 = isEmpty empty
prop_empty_numKnots                = numKnots           empty == 0
prop_empty_numDistinctKnots        = numDistinctKnots   empty == 0
prop_empty_knotMultiplicity      x = knotMultiplicity x empty == 0
prop_empty_valid                   = valid empty

-- knot
knot_tests =
    [ testProperty "round-trip via knots"           prop_knot_knots_roundTrip
    , testProperty "round-trip via distinctKnots"   prop_knot_distinctKnots_roundTrip
    , testProperty "round-trip via toList"          prop_knot_toList_roundTrip
    , testProperty "round-trip via toMap"           prop_knot_toMap_roundTrip
    , testProperty "isEmpty"                        prop_knot_isEmpty
    , testProperty "numKnots"                       prop_knot_numKnots
    , testProperty "numDistinctKnots"               prop_knot_numDistinctKnots
    , testProperty "knotMultiplicity"               prop_knot_knotMultiplicity
    , testProperty "valid"                          prop_knot_valid
    ]

prop_knot_knots_roundTrip           x = knots              (knot x) == [x]
prop_knot_distinctKnots_roundTrip   x = distinctKnots      (knot x) == [x]
prop_knot_toList_roundTrip          x = toList             (knot x) == [(x,1)]
prop_knot_toMap_roundTrip           x = toMap              (knot x) == M.singleton x 1
prop_knot_isEmpty                   x = isEmpty            (knot x) == False
prop_knot_numKnots                  x = numKnots           (knot x) == 1
prop_knot_numDistinctKnots          x = numDistinctKnots   (knot x) == 1
prop_knot_knotMultiplicity        x y = knotMultiplicity x (knot x) == 1
                                     && knotMultiplicity y (knot x) == if x == y then 1 else 0
prop_knot_valid                     x = valid (knot x)

-- multipleKnot
multipleKnot_tests = 
    [ testProperty "round-trip via knots"           prop_multipleKnot_knots_roundTrip
    , testProperty "round-trip via distinctKnots"   prop_multipleKnot_distinctKnots_roundTrip
    , testProperty "round-trip via toList"          prop_multipleKnot_toList_roundTrip
    , testProperty "round-trip via toMap"           prop_multipleKnot_toMap_roundTrip
    , testProperty "isEmpty"                        prop_multipleKnot_isEmpty
    , testProperty "numKnots"                       prop_multipleKnot_numKnots
    , testProperty "numDistinctKnots"               prop_multipleKnot_numDistinctKnots
    , testProperty "knotMultiplicity"               prop_multipleKnot_knotMultiplicity
    , testProperty "valid"                          prop_multipleKnot_valid
    ]

prop_multipleKnot_knots_roundTrip           x (Multiplicity n) =
    knots         (multipleKnot x n) == replicate (max 0 n) x

prop_multipleKnot_distinctKnots_roundTrip   x (Multiplicity n) =
    distinctKnots (multipleKnot x n) == [x | n > 0]

prop_multipleKnot_toList_roundTrip          x (Multiplicity n) =
    toList        (multipleKnot x n) == [(x,n) | n > 0]

prop_multipleKnot_toMap_roundTrip           x (Multiplicity n) =
    toMap         (multipleKnot x n) == M.fromList [(x,n) | n > 0]

prop_multipleKnot_isEmpty                   x (Multiplicity n) =
    isEmpty (multipleKnot x n) == (n <= 0)

prop_multipleKnot_numKnots                  x (Multiplicity n) =
    numKnots (multipleKnot x n) == max 0 n

prop_multipleKnot_numDistinctKnots          x (Multiplicity n) =
    numDistinctKnots (multipleKnot x n) == if n > 0 then 1 else 0

prop_multipleKnot_knotMultiplicity x y (Multiplicity n)
    =  knotMultiplicity x (multipleKnot x n) == max 0 n
    && knotMultiplicity y (multipleKnot x n) == if x == y then max 0 n else 0

prop_multipleKnot_valid x (Multiplicity n)
    = valid (multipleKnot x n)

-- mkKnots
mkKnots_tests =
    [ testProperty "round-trip via knots"           prop_mkKnots_knots_roundTrip
    , testProperty "round-trip via distinctKnots"   prop_mkKnots_distinctKnots_roundTrip
    , testProperty "round-trip via toList"          prop_mkKnots_toList_roundTrip
    , testProperty "round-trip via toMap"           prop_mkKnots_toMap_roundTrip
    , testProperty "isEmpty"                        prop_mkKnots_isEmpty
    , testProperty "numKnots"                       prop_mkKnots_numKnots
    , testProperty "numDistinctKnots"               prop_mkKnots_numDistinctKnots
    , testProperty "knotMultiplicity"               prop_mkKnots_knotMultiplicity
    , testProperty "valid"                          prop_mkKnots_valid
    ]

prop_mkKnots_knots_roundTrip         kts = knots              (mkKnots kts) == sort kts
prop_mkKnots_distinctKnots_roundTrip kts = distinctKnots      (mkKnots kts) == uniq (sort kts)
prop_mkKnots_toList_roundTrip        kts = toList             (mkKnots kts) == map (head &&& length) (group (sort kts))
prop_mkKnots_toMap_roundTrip         kts = toMap              (mkKnots kts) == M.fromListWith (+) (map (\k -> (k,1)) kts)
prop_mkKnots_isEmpty                 kts = isEmpty            (mkKnots kts) == null kts
prop_mkKnots_numKnots                kts = numKnots           (mkKnots kts) == length kts
prop_mkKnots_numDistinctKnots        kts = numDistinctKnots   (mkKnots kts) == length (uniq (sort kts))
prop_mkKnots_knotMultiplicity      x kts = knotMultiplicity x (mkKnots kts) == count (x==) kts
prop_mkKnots_valid                   kts = valid (mkKnots kts)

uniq []     = []
uniq (x:xs) = x : uniq (dropWhile (x==) xs)

count p = length . filter p

-- fromList
fromList_tests =
    [ testProperty "round-trip via knots"           prop_fromList_knots_roundTrip
    , testProperty "round-trip via distinctKnots"   prop_fromList_distinctKnots_roundTrip
    , testProperty "round-trip via toList"          prop_fromList_toList_roundTrip
    , testProperty "round-trip via toMap"           prop_fromList_toMap_roundTrip
    , testProperty "isEmpty"                        prop_fromList_isEmpty
    , testProperty "numKnots"                       prop_fromList_numKnots
    , testProperty "numDistinctKnots"               prop_fromList_numDistinctKnots
    , testProperty "knotMultiplicity"               prop_fromList_knotMultiplicity
    , testProperty "valid"                          prop_fromList_valid
    ]

prop_fromList_knots_roundTrip         (KnotList kts)
    =  knots (fromList kts)
    == sort (concat [replicate n x | (x, n) <- kts])
prop_fromList_distinctKnots_roundTrip (KnotList kts) 
    =  distinctKnots (fromList kts) 
    == uniq (sort [x | (x, n) <- kts, n > 0])
prop_fromList_toList_roundTrip        (KnotList kts)
    = toList (fromList kts)
    == coalesce [(x, n) | (x, n) <- kts, n > 0]
    where
        -- Given a list of key,count pairs, combine all entries for the same
        -- keys and return the result by ascending order of key
        coalesce = M.toList . M.fromListWith (+)

prop_fromList_toMap_roundTrip         (KnotList kts)
    =  toMap (fromList kts)
    == M.fromListWith (+) (filter ((>0).snd) kts)

prop_fromList_isEmpty                 (KnotList kts)
    =  isEmpty (fromList kts)
    == null [() | (_,n) <- kts, n > 0]

prop_fromList_numKnots                (KnotList kts)
    =  numKnots (fromList kts)
    == sum [n | (_,n) <- kts, n > 0]
prop_fromList_numDistinctKnots        (KnotList kts)
    =  numDistinctKnots (fromList kts)
    == length (uniq (sort [x | (x,n) <- kts, n > 0]))

prop_fromList_knotMultiplicity x (KnotList kts)
    =  knotMultiplicity x (fromList kts)
    == sum [ n | (y,n) <- kts, n > 0, x == y]

prop_fromList_valid (KnotList kts) = valid (fromList kts)

-- fromMap
fromMap_tests =
    [ testProperty "round-trip via knots"           prop_fromMap_knots_roundTrip
    , testProperty "round-trip via distinctKnots"   prop_fromMap_distinctKnots_roundTrip
    , testProperty "round-trip via toList"          prop_fromMap_toList_roundTrip
    , testProperty "round-trip via toMap"           prop_fromMap_toMap_roundTrip
    , testProperty "isEmpty"                        prop_fromMap_isEmpty
    , testProperty "numKnots"                       prop_fromMap_numKnots
    , testProperty "numDistinctKnots"               prop_fromMap_numDistinctKnots
    , testProperty "knotMultiplicity"               prop_fromMap_knotMultiplicity
    , testProperty "valid"                          prop_fromMap_valid
    ]

prop_fromMap_knots_roundTrip (KnotMap kts)
    =  knots (fromMap kts)
    == concat [replicate n x | (x, n) <- M.toList kts, n > 0]
prop_fromMap_distinctKnots_roundTrip (KnotMap kts) 
    =  distinctKnots (fromMap kts) 
    == M.keys (M.filter (>0) kts)
prop_fromMap_toList_roundTrip (KnotMap kts)
    =  toList (fromMap kts)
    == [(x, n) | (x, n) <- M.toList kts, n > 0]
prop_fromMap_toMap_roundTrip (KnotMap kts)
    =  toMap (fromMap kts)
    == M.filter (>0) kts

prop_fromMap_isEmpty (KnotMap kts)
    =  isEmpty (fromMap kts)
    == M.null (M.filter (>0) kts)

prop_fromMap_numKnots (KnotMap kts)
    =  numKnots (fromMap kts)
    == sum [n | (_,n) <- M.toList kts, n > 0]
prop_fromMap_numDistinctKnots (KnotMap kts)
    =  numDistinctKnots (fromMap kts)
    == M.size (M.filter (>0) kts)

prop_fromMap_knotMultiplicity x (KnotMap kts)
    =  knotMultiplicity x (fromMap kts)
    == maybe 0 (max 0) (M.lookup x kts)

prop_fromMap_valid (KnotMap kts) = M.valid kts ==> valid (fromMap kts)

-- fromAscList
fromAscList_tests =
    [ testProperty "sane"   prop_fromAscList_sane
    , testProperty "valid"  prop_fromAscList_valid
    ]

prop_fromAscList_sane (KnotList kts)
    =  fromAscList (sortBy (comparing fst) kts)
    == fromList kts

-- Sorted input is a sufficient (but perhaps not necessary) condition for validity
prop_fromAscList_valid (KnotList kts)
    =  valid (fromAscList kts)
    >= sortedBy (comparing fst) kts

sortedBy cmp xs = (xs == sortBy cmp xs)

-- fromDistinctAscList
fromDistinctAscList_tests =
    [ testProperty "sane"  prop_fromDistinctAscList_sane
    , testProperty "valid" prop_fromDistinctAscList_valid
    ]

prop_fromDistinctAscList_sane (KnotList rawKts)
    =  fromDistinctAscList (sortBy (comparing fst) kts)
    == fromList kts
    where kts = nubBy ((==) `on` fst) rawKts

-- sorted and distinct input is a sufficient (but perhaps not necessary) condition for validity
prop_fromDistinctAscList_valid (KnotList kts)
    =  valid (fromDistinctAscList kts)
    >= (sortedBy (comparing fst) kts && distinctBy ((==) `on` fst) kts)

distinctBy (==) xs = and [not (x0 == x1) | (x0,x1) <- pairs xs]

-- * Destructors
-- knots
-- numKnots
-- toList
-- toMap
-- distinctKnots
-- numDistinctKnots
-- knotMultiplicity
-- valid

-- * Knot vector manipulations

-- splitLookup
splitLookup_tests =
    [ testProperty "output disjoint and ordered"            prop_splitLookup_output_disjointOrdered
    , testProperty "splits input"                           prop_splitLookup_splits_input
    , testProperty "locates n'th knot"                      prop_splitLookup_locates_knot
    ]

prop_splitLookup_output_disjointOrdered k kts = and
    [ disjointOrdered (M.keysSet m1) (M.keysSet m2)
    | (m1,m2) <- pairs (splitLookup' k kts)]
prop_splitLookup_splits_input    k kts
    =  M.unions (splitLookup' k kts)
    == toMap kts
prop_splitLookup_locates_knot k kts =
    numKnots kts > 0 ==>
        let i = k `mod` numKnots kts
            (_, mbX, _) = splitLookup i kts
            x = knots kts !! i
         in mbX == Just (x, knotMultiplicity x kts)

pairs xs = [(y,z) | (y:ys) <- tails xs, z <- ys]
disjointOrdered s1 s2 = or
    [ S.null s1
    , S.null s2
    , S.findMax s1 < S.findMin s2
    ]

-- convenience wrapper for testing; calls splitLookup then converts all three
-- pieces of the result to M.Map k Int
splitLookup' k kts = 
    [ toMap pre
    , maybe M.empty (uncurry M.singleton) mbX
    , toMap post
    ] where (pre, mbX, post) = splitLookup k kts

-- takeKnots
takeKnots_tests =
    [ testProperty "definition"  prop_takeKnots_definition
    ]

prop_takeKnots_definition n kts
    =  knots (takeKnots n kts)
    == take n (knots kts)

-- dropKnots
dropKnots_tests =
    [ testProperty "definition"  prop_dropKnots_definition
    ]

prop_dropKnots_definition n kts
    =  knots (dropKnots n kts)
    == drop n (knots kts)

-- splitKnotsAt
splitKnotsAt_tests =
    [ testProperty "definition"  prop_splitKnotsAt_definition
    ]

prop_splitKnotsAt_definition n kts
    =  let (ks1, ks2) = splitKnotsAt n kts in (knots ks1, knots ks2)
    == splitAt n (knots kts)

-- takeDistinctKnots
takeDistinctKnots_tests =
    [ testProperty "definition" prop_takeDistinctKnots_definition
    ]

prop_takeDistinctKnots_definition n kts
    =  knots (takeDistinctKnots n kts)
    == takeDistinct n (knots kts)

takeDistinct n = concat . take n . group

-- dropDistinctKnots
dropDistinctKnots_tests =
    [ testProperty "definition" prop_dropDistinctKnots_definition
    ]

prop_dropDistinctKnots_definition n kts
    =  knots (dropDistinctKnots n kts)
    == dropDistinct n (knots kts)

dropDistinct n = concat . drop n . group

-- splitDistinctKnotsAt
splitDistinctKnotsAt_tests =
    [ testProperty "definition" prop_splitDistinctKnotsAt_definition
    ]

prop_splitDistinctKnotsAt_definition n kts
    =  let (ks1, ks2) = splitDistinctKnotsAt n kts
        in (knots ks1, knots ks2)
    == (takeDistinct n (knots kts), dropDistinct n (knots kts))

-- setKnotMultiplicity
setKnotMultiplicity_tests =
    [ testProperty "Sets multiplicity"              prop_setKnotMultiplicity_sets_multiplicity
    , testProperty "Preserves other multiplicities" prop_setKnotMultiplicity_preserves_others
    ]

prop_setKnotMultiplicity_sets_multiplicity kts x (Multiplicity m)
    =  knotMultiplicity x (setKnotMultiplicity x m kts) == max 0 m
prop_setKnotMultiplicity_preserves_others kts x (Multiplicity m) = 
    numKnots kts > 0 ==> do
        checkExistingKnot <- arbitrary
        y <- if checkExistingKnot 
            then do
                n <- arbitrary
                let k = n `mod` numKnots kts
                return (knots kts !! k)
            else arbitrary
        property  (knotMultiplicity y (setKnotMultiplicity x m kts)
                == if x == y then max m 0 else knotMultiplicity y kts)

-- lookupKnot
lookupKnot_tests =
    [ testProperty "definition" prop_lookupKnot_definition
    ]

prop_lookupKnot_definition kts n
    =  lookupKnot n kts
    == index n (knots kts)

index n xs = do
    guard (n >= 0)
    listToMaybe (drop n xs)

lookupDistinctKnot_tests =
    [ testProperty "definition" prop_lookupDistinctKnot_definition
    ]

prop_lookupDistinctKnot_definition kts n
    | n < 0     = lookupDistinctKnot n kts == Nothing
    | otherwise = lookupDistinctKnot n kts == listToMaybe (drop n (distinctKnots kts))

-- knotDomain
-- 
-- uniform
