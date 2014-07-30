-- Copyright 2014 Romain Edelmann. All rights reserved.

-- | This modules provides various measures on
--   finite discrete probability distributions.
module Data.Distribution.Measure
    ( -- * Measures
      -- ** Probability
      probability
    , probabilityAt
    , probabilityIn
      -- ** Expectation
    , expectation
    , mean
      -- ** Variation
    , variance
    , standardDeviation
      -- ** Values
    , median
    , modes
    , quantile
    ) where

import Control.Arrow (second)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Data.Distribution.Core


-- | Probability that a predicate holds on the distribution.
--
--   >>> probability (\ x -> x == 1 || x == 6) $ uniform [1 .. 6]
--   1 % 3
--
--   Takes @O(n)@ time. See 'probabilityAt' and 'probabilityIn'
--   for a more efficient ways to query elements and ranges.
probability :: (a -> Bool) -> Distribution a -> Probability
probability f = sum . Map.elems . Map.filterWithKey (const . f) . toMap

-- | Probability of a given value.
--
--   Takes @O(log(n))@ time.
probabilityAt :: Ord a => a -> Distribution a -> Probability
probabilityAt x d = case Map.lookup x (toMap d) of
    Just p -> p
    Nothing -> 0

-- | Probability of a the inclusive @[low, high]@ range.
--   When @low > high@, the probability is 0.
--
--   Takes @O(log(n) + m)@ time, where @n@ is the size of
--   the distribution and @m@ the size of the range.
probabilityIn :: Ord a => (a, a) -> Distribution a -> Probability
probabilityIn (low, high) d
    | low > high = 0
    | low == high = probabilityAt low d
    | otherwise = Map.foldl' (+) (ph + pl) ps
  where
    (_, ml, hs) = Map.splitLookup low $ toMap d
    (ps, mh, _) = Map.splitLookup high hs

    pl = fromMaybe 0 ml
    ph = fromMaybe 0 mh

-- | Returns the expectation, or mean, of a distribution.
--
-- >>> expectation $ uniform [0, 1]
-- 0.5
--
-- Empty distributions have an expectation of @0@.
expectation :: (Real a, Fractional b) => Distribution a -> b
expectation = fromRational . sum .
    fmap (uncurry (*) . second toRational) .
    Map.toList . Map.mapKeysWith (+) toRational . toMap

-- | Returns the variance of a distribution.
--
-- >>> variance $ always 1
-- 0.0
-- >>> variance $ uniform [0 .. 1]
-- 0.25
-- >>> variance $ uniform [1 .. 7]
-- 4.0
--
-- Empty distributions have a variance of @0@.
variance :: (Real a, Fractional b) => Distribution a -> b
variance d = expectation dSquare - (e * e)
  where
    e = expectation d
    dSquare = select (square . toRational) d
    square x = x * x

-- | Standard deviation.
--
--   >>> standardDeviation $ always 1
--   0.0
--   >>> standardDeviation $ uniform [0 .. 1]
--   0.5
--   >>> standardDeviation $ uniform [1 .. 7]
--   2.0
standardDeviation :: (Real a, Floating b) => Distribution a -> b
standardDeviation = sqrt . fromRational . variance

-- | Returns the smallest value in the distribution such that
--   at least a fraction `p` of the values are less or equal to it.
--
--   >>> quantile 0.0 $ uniform [1, 2, 3]
--   Just 1
--   >>> quantile 0.5 $ uniform [1, 2, 3]
--   Just 2
--   >>> quantile 1.0 $ uniform [1, 2, 3]
--   Just 3
--   >>> quantile 0.5 $ fromList []
--   Nothing
quantile :: Probability -> Distribution a -> Maybe a
quantile p d = case dropWhile ((< r) . snd) $ scanl1 go $ toList d of
    (x, _) : _ -> Just x
    _          -> Nothing
  where
    r = max 0 $ min 1 p
    go (_, q') (x, q) = (x, q' + q)

-- | Returns the median of the values.
--   The median is the smallest value such that at least 50% of
--   the values are less to it.
--
--   >>> median $ fromList [(1, 0.6), (2, 0.4)]
--   Just 1
--   >>> median $ fromList [(1, 0.4), (2, 0.6)]
--   Just 2
median :: Distribution a -> Maybe a
median = quantile 0.5

-- | Synonym of 'expectation'.
mean :: (Real a, Fractional b) => Distribution a -> b
mean = expectation

-- | Returns all values whose probability is maximal.
modes :: Distribution a -> [a]
modes d = map fst $ filter ((m ==) . snd) xs
  where
    xs = toList d
    m = maximum $ map snd xs
