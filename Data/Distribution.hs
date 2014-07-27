{-# LANGUAGE MultiWayIf #-}

-- Copyright 2014 Romain Edelmann. All rights reserved.

-- | This modules defines types and functions for manipulating
--   finite discrete probability distributions.
--
--   This module is best suited for work on small to medium
--   distributions.
module Data.Distribution
    ( -- * Distribution
      Distribution
    , toList
    , Probability
      -- ** Creation
    , always
    , uniform
    , fromList
    , chance
    , trials
    , times
      -- ** Transformation
    , select
    , assuming
      -- ** Chaining
    , andThen
    , on
      -- ** Measures
    , size
    , support
    , probability
    , probabilityAt
    , expectation
    , variance
    , standardDeviation
    , mean
    , median
    , modes
    , quantile
      -- ** Aggregation
    , Aggregator
    , aggregateWith
      -- *** Useful aggregators
    , cumulative
    , decreasing
    , complementary
    ) where

import Control.Arrow (second)
import qualified Data.Function as F
import Data.List (foldl', groupBy, sortBy, find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Set (Set)

-- | Distribution over values of type @a@.
--
--   Due to their internal representations, @Distribution@ can not have
--   @Functor@ or @Monad@ instances.
--   However, 'select' is the equivalent of @fmap@ for distributions
--   and 'always' and 'andThen' are respectively the equivalent of @return@
--   and @>>=@.
newtype Distribution a = Distribution { getDistribution :: Map a Probability }
    deriving Eq

instance Show a => Show (Distribution a) where
    show d = "fromList " ++ show (toList d)

-- A distribution @d1@ is less than some other distribution @d2@
-- if the smallest value that has a different probability
-- in @d1@ and @d2@ is more probable in @d1@.
--
-- By convention, empty distributions are less than
-- everything except themselves.
instance Ord a => Ord (Distribution a) where
    compare d1 d2 = case (toList d1, toList d2) of
        ([], []) -> EQ
        ([], _)  -> LT
        (_, [])  -> GT
        (xs, ys) -> case find (uncurry (/=)) $ zip xs ys of
            Nothing -> EQ
            Just ((x, p), (y, q)) -> case compare x y of
                EQ -> compare q p
                c  -> c

-- Lifts the bounds to the distributions that return them
-- with probability one.
--
-- Note that the degenerate distributions of size @0@ will
-- be less than the distribution @minBound@.
--
-- Appart from that, all other distributions d have
-- the property that @minBound <= d <= maxBound@ if
-- this property holds on the values of the distribution.
instance Bounded a => Bounded (Distribution a) where
    minBound = always minBound
    maxBound = always maxBound

-- Binary operations on distributions are defined to
-- be the binary operation on each pair of elements.
--
-- For this reason, @(+)@ and @(*)@ are not related in the same way
-- as they are on natural numbers.
--
-- For instance, it is not always the case that:
-- @3 * d == d + d + d@
--
-- >>> let d = uniform [0, 1]
-- >>> 3 * d
-- fromList [(0,1 % 2),(3,1 % 2)]
-- >>> d + d + d
-- fromList [(0,1 % 8),(1,3 % 8),(2,3 % 8),(3,1 % 8)]
--
-- For this particular behavior, see the `times` function.
instance (Ord a, Num a) => Num (Distribution a) where
    fromInteger = always . fromInteger
    abs = select abs
    signum = select signum
    negate = select negate
    d1 + d2 = d1 `andThen` (+) `on` d2
    d1 - d2 = d1 `andThen` (-) `on` d2
    d1 * d2 = d1 `andThen` (*) `on` d2

-- Binary operations on distributions are defined to
-- be the binary operation on each pair of elements.
instance (Ord a, Fractional a) => Fractional (Distribution a) where
    fromRational = always . fromRational
    d1 / d2 = d1 `andThen` (/) `on` d2
    recip = select recip

-- Binary operations on distributions are defined to
-- be the binary operation on each pair of element.
instance (Ord a, Floating a) => Floating (Distribution a) where
    pi = always pi
    exp = select exp
    sqrt = select sqrt
    log = select log
    d1 ** d2 = d1 `andThen` (**) `on` d2
    d1 `logBase` d2 = d1 `andThen` logBase `on` d2
    sin = select sin
    tan = select tan
    cos = select cos
    asin = select asin
    atan = select atan
    acos = select acos
    sinh = select sinh
    tanh = select tanh
    cosh = select cosh
    asinh = select asinh
    atanh = select atanh
    acosh = select acosh


-- | Converts the distribution to a list of increasing values whose probability
--   is greater than @0@. To each value is associated its probability.
toList :: Distribution a -> [(a, Probability)]
toList (Distribution xs) = Map.toAscList xs


-- | Probability. Should be between 0 and 1.
type Probability = Rational

-- | Distribution that assigns to @x@ the probability of @1@.
--
-- >>> probability (== 0) $ always 0
-- 1 % 1
-- >>> probability (/= 0) $ always 0
-- 0 % 1
always :: a -> Distribution a
always x = Distribution $ Map.singleton x 1

-- | Distribution that assigns to each @value@ from the given @(value, weight)@
--   pairs a probability proportional to @weight@.
--
--   >>> probability (> 'A') $ fromList [('A', 1), ('B', 2), ('C', 1)]
--   3 % 4
--
--   Values may appear multiple times in the list. In this case, their total
--   weight is the sum of the different associated weights.
--   Values whose total weight is zero or negative are ignored.
fromList :: (Ord a, Real p) => [(a, p)] -> Distribution a
fromList xs = Distribution $ Map.fromAscList $ zip vs scaledPs
  where
    as = map aggregate $ groupBy ((==) `F.on` fst) $ sortBy (comparing fst) xs
      where
        aggregate ys = let (v : _, qs) = unzip ys in
            (v, fromRational $ toRational $ sum qs)
    (vs, ps) = unzip $ filter ((> 0) . snd) as
    t = sum ps
    scaledPs = if t /= 1 then map (/ t) ps else ps

-- | Uniform distribution over the values.
--   The probability of each element is proportional
--   to its number of appearance in the list.
--
--   >>> probability (== 6) $ uniform [1 .. 6]
--   1 % 6
uniform :: (Ord a) => [a] -> Distribution a
uniform xs = fromList $ fmap (\ x -> (x, p)) xs
  where
    p = 1 / toRational (length xs)

-- | @True@ with given probability and @False@ with complementary probability.
chance :: Real p => p -> Distribution Bool
chance p = fromList [(False, 1 - p'), (True, p')]
  where
    p' = fromRational $ max 0 $ min 1 $ toRational p

-- | Binomial distribution.
--   Assigns for each number of successes its probability.
--
--   >>> probability (== 1) $ trials 2 $ uniform [True, False]
--   1 % 2
trials :: Int -> Distribution Bool -> Distribution Int
trials n d = Distribution $ Map.fromAscList $ if
    | p == 1    -> [(n, 1)]
    | p == 0    -> [(0, 1)]
    | otherwise -> zip outcomes probs
  where
    p = probability (== True) d
    q = 1 - p

    ps = take (n + 1) $ iterate (* p) 1
    qs = reverse $ take (n + 1) $ iterate (* q) 1

    probs = zipWith (*) pascalRow $ zipWith (*) ps qs

    outcomes = [0 .. n]

    pascalRow = fmap (fromRational . toRational) $
        scanl ( \ c k -> c * (n' + 1 - k) `div` k) 1 [1 .. n']
      where
        n' = toInteger n

-- | Takes `n` samples from the distribution and returns the distribution
--   of their sum.
--
--   >>> probability (> 12) $ 3 `times` uniform [1 .. 6]
--   7 % 27
--
--   This function makes use of the more efficient @trials@ functions
--   for input distributions of size @2@.
--
--   >>> fromRational $ probability (> 5600) $ 1000 `times` uniform [1, 10]
--   0.23352256082639306
times :: (Num a, Ord a) => Int -> Distribution a -> Distribution a
n `times` d
    | n <= 0 = 0
    | s == 0 = d
    | s == 1 = select (* n') d
    | s == 2 = case toList d of  -- Performs Bernoulli trials. (efficiency)
        [(a, p), (b, q)] -> select (go a b) $ trials n $
            fromList [(True, p), (False, q)]
        _ -> error "times: size seems not to be properly defined."
    | otherwise = sum $ replicate n d
  where
    s = size d
    n' = fromInteger $ toInteger n
    go a b k = k' * a + (n' - k') * b
      where
        k' = fromInteger $ toInteger k


-- Transformations


-- | Applies a function to the values in the distribution.
--
--   >>> probability (== 1) $ select abs $ uniform [-1, 0, 1]
--   2 % 3
select :: Ord b => (a -> b) -> Distribution a -> Distribution b
select f (Distribution xs) = Distribution $ Map.mapKeysWith (+) f xs

-- | Returns a new distribution conditioning on the predicate holding
--   on the value.
--
--   >>> let d = assuming (> 2) $ uniform [1 .. 6]
--   >>> probability (== 2) d
--   0 % 1
--   >>> probability (== 3) d
--   1 % 4
assuming :: (a -> Bool) -> Distribution a -> Distribution a
assuming f (Distribution xs) = Distribution $ fmap adjust filtered
  where
    filtered = Map.filterWithKey (const . f) xs
    adjust x = x * (1 / total)
    total = sum $ Map.elems filtered


-- Chaining


-- | Computes for each value in the distribution a new distribution, and then
--   combines those distributions, giving each the weight of the original value.
--
--   >>> let d = uniform [1 .. 6] `andThen` (\ n -> uniform [1 .. n])
--   >>> probability (== 6) d
--   1 % 36
--   >>> probability (== 1) d
--   49 % 120
--
--   See the 'on' function for a convenient way to chain distributions.
infixl 7 `andThen`
andThen :: Ord b => Distribution a -> (a -> Distribution b) -> Distribution b
andThen (Distribution xs) f = Distribution $
    foldl' (Map.unionWith (+)) Map.empty $ fmap go $ Map.toList xs
  where
    go (x, p) = fmap (* p) $ getDistribution $ f x

-- | Utility to partially apply a function on a distribution.
--   A use case for 'on' is to use in conjunction with 'andThen'
--   to combine distributions.
--
--   >>> let d = uniform [1 .. 6] `andThen` (+) `on` uniform [1 .. 6]
--   >>> probability (== 12) d
--   1 % 36
infixl 8 `on`
on :: Ord c => (a -> b -> c) -> Distribution b -> a -> Distribution c
on f d x = select (f x) d


-- Measures


-- | Returns the number of elements with non-zero probability
--   in the distribution.
size :: Distribution a -> Int
size (Distribution xs) = Map.size xs

-- | Probability that a predicate holds on the distribution.
--
--   >>> probability (\ x -> x == 1 || x == 6) $ uniform [1 .. 6]
--   1 % 3
probability :: (a -> Bool) -> Distribution a -> Probability
probability f (Distribution xs) = sum $ Map.elems $
    Map.filterWithKey (const . f) xs

-- | Probability of a given value.
probabilityAt :: Ord a => a -> Distribution a -> Probability
probabilityAt x (Distribution xs) = case Map.lookup x xs of
    Just p -> p
    Nothing -> 0

-- | Returns the expectation, or mean, of a distribution.
--
-- >>> expectation $ uniform [0, 1]
-- 0.5
--
-- Empty distributions have an expectation of @0@.
expectation :: (Real a, Fractional b) => Distribution a -> b
expectation (Distribution xs) = fromRational $ sum $
    fmap (uncurry (*) . second toRational) $
    Map.toList $ Map.mapKeysWith (+) toRational xs

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
quantile p d = case dropWhile ((< r) . snd) $ aggregateWith cumulative $ toList d of
    (x, _) : _ -> Just x
    _          -> Nothing
  where
    r = max 0 $ min 1 p

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
modes (Distribution xs) = map fst $ filter ((m ==) . snd) ys
  where
    ys = Map.toAscList xs
    m = maximum $ Map.elems xs

-- | Values in the distribution with non-zero probability.
support :: Distribution a -> Set a
support (Distribution xs) = Map.keysSet xs


-- Aggregation


-- | Functions that operate on probabilities.
type Aggregator = [Probability] -> [Probability]

-- | Applies an aggregator on a list of values tagged with their probability.
aggregateWith :: Aggregator -> [(a, Probability)] -> [(a, Probability)]
aggregateWith f xs = zip vs $ f ps
  where
    (vs, ps) = unzip xs

-- | Adds to each probability the sum of the probabilities earlier in the list.
cumulative :: Aggregator
cumulative = scanl1 (+)

-- | Replaces each probability by its complement.
complementary :: Aggregator
complementary = map (1 -)

-- | Adds to each probability the sum of probabilities later in the list.
decreasing :: Aggregator
decreasing = init . (1 :) . complementary . cumulative

