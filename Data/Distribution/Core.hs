{-# LANGUAGE MultiWayIf #-}

{- Copyright 2014 Romain Edelmann

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. -}


-- | This modules defines types and functions for manipulating
--   finite discrete probability distributions.
module Data.Distribution.Core
    ( -- * Probability
      Probability
      -- * Distribution
    , Distribution
    , toMap
    , toList
      -- ** Properties
    , size
    , support
      -- ** Creation
    , fromList
    , always
    , uniform
    , withProbability
      -- ** Transformation
    , select
    , assuming
    , observing
      -- ** Combination
    , combineWith
      -- ** Sequences
      -- *** Independant experiments
    , trials
    , times
    , iid
      -- *** Dependant experiments
    , andThen
      -- ** Utilities
    , isValid
    ) where

import Control.Arrow (second)
import qualified Data.Function as F
import Data.List (tails, groupBy, sortBy, find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Ord (comparing)
import Data.Set (Set)


-- | Probability. Should be between 0 and 1.
type Probability = Rational

-- | Distribution over values of type @a@.
--
--   Due to their internal representations, @Distribution@ can not have
--   @Functor@ or @Monad@ instances.
--   However, 'select' is the equivalent of @fmap@ for distributions
--   and 'always' and 'andThen' are respectively the equivalent of @return@
--   and @>>=@.
newtype Distribution a = Distribution
    { toMap :: Map a Probability
      -- ^ Converts the distribution to a mapping from values to their
      --   probability. Values with probability @0@ are not included
      --   in the resulting mapping.
    } deriving Eq

instance Show a => Show (Distribution a) where
    show d = "fromList " ++ show (toList d)

-- | A distribution @d1@ is less than some other distribution @d2@
--   if the smallest value that has a different probability
--   in @d1@ and @d2@ is more probable in @d1@.
--
--   By convention, empty distributions are less than
--   everything except themselves.
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

-- | Lifts the bounds to the distributions that return them
--   with probability one.
--
--   Note that the degenerate distributions of size @0@ will
--   be less than the distribution @minBound@.
--
--   Appart from that, all other distributions d have
--   the property that @minBound <= d <= maxBound@ if
--   this property holds on the values of the distribution.
instance Bounded a => Bounded (Distribution a) where
    minBound = always minBound
    maxBound = always maxBound

-- | Literals are interpreted as distributions that always
--   return the given value.
--
--   >>> 42 == always 42
--   True
--
--   Binary operations on distributions are defined to
--   be the binary operation on each pair of elements.
--
--   For this reason, @(+)@ and @(*)@ are not related in the same way
--   as they are on natural numbers.
--
--   For instance, it is not always the case that:
--   @3 * d == d + d + d@
--
--   >>> let d = uniform [0, 1]
--   >>> 3 * d
--   fromList [(0,1 % 2),(3,1 % 2)]
--   >>> d + d + d
--   fromList [(0,1 % 8),(1,3 % 8),(2,3 % 8),(3,1 % 8)]
--
--   For this particular behavior, see the `times` function.
instance (Ord a, Num a) => Num (Distribution a) where
    fromInteger = always . fromInteger
    abs = select abs
    signum = select signum
    negate = select negate
    d1 + d2 = combineWith (+) d1 d2
    d1 - d2 = combineWith (-) d1 d2
    d1 * d2 = combineWith (*) d1 d2

-- Binary operations on distributions are defined to
-- be the binary operation on each pair of elements.
instance (Ord a, Fractional a) => Fractional (Distribution a) where
    fromRational = always . fromRational
    d1 / d2 = combineWith (+) d1 d2
    recip = select recip

-- Binary operations on distributions are defined to
-- be the binary operation on each pair of element.
instance (Ord a, Floating a) => Floating (Distribution a) where
    pi = always pi
    exp = select exp
    sqrt = select sqrt
    log = select log
    d1 ** d2 = combineWith (**) d1 d2
    d1 `logBase` d2 = combineWith logBase d1 d2
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

instance (Ord a, Semigroup a) => Semigroup (Distribution a) where
    d1 <> d2 = combineWith (<>) d1 d2

instance (Ord a, Monoid a) => Monoid (Distribution a) where
    mempty = always mempty
    mappend = (<>)

-- | Converts the distribution to a list of increasing values whose probability
--   is greater than @0@. To each value is associated its probability.
toList :: Distribution a -> [(a, Probability)]
toList (Distribution xs) = Map.toAscList xs


-- Properties


-- | Returns the number of elements with non-zero probability
--   in the distribution.
size :: Distribution a -> Int
size = Map.size . toMap

-- | Values in the distribution with non-zero probability.
support :: Distribution a -> Set a
support = Map.keysSet . toMap


-- Creation


-- | Distribution that assigns to each @value@ from the given @(value, weight)@
--   pairs a probability proportional to @weight@.
--
--   >>> fromList [('A', 1), ('B', 2), ('C', 1)]
--   fromList [('A',1 % 4),('B',1 % 2),('C',1 % 4)]
--
--   Values may appear multiple times in the list. In this case, their total
--   weight is the sum of the different associated weights.
--   Values whose total weight is zero or negative are ignored.
fromList :: (Ord a, Real p) => [(a, p)] -> Distribution a
fromList xs = Distribution $ Map.fromDistinctAscList $ zip vs scaledPs
  where
    as = map aggregate $ groupBy ((==) `F.on` fst) $ sortBy (comparing fst) xs
      where
        aggregate ys = let (v : _, qs) = unzip ys in
            (v, fromRational $ toRational $ sum qs)
    (vs, ps) = unzip $ filter ((> 0) . snd) as
    t = sum ps
    scaledPs = if t /= 1 then map (/ t) ps else ps

-- | Distribution that assigns to @x@ the probability of @1@.
--
-- >>> always 0
-- fromList [(0,1 % 1)]
--
-- >>> always 42
-- fromList [(42,1 % 1)]
always :: a -> Distribution a
always x = Distribution $ Map.singleton x 1

-- | Uniform distribution over the values.
--   The probability of each element is proportional
--   to its number of appearance in the list.
--
--   >>> uniform [1 .. 6]
--   fromList [(1,1 % 6),(2,1 % 6),(3,1 % 6),(4,1 % 6),(5,1 % 6),(6,1 % 6)]
uniform :: (Ord a) => [a] -> Distribution a
uniform xs = fromList $ fmap (\ x -> (x, p)) xs
  where
    p = 1 / toRational (length xs)

-- | @True@ with given probability and @False@ with complementary probability.
withProbability :: Real p => p -> Distribution Bool
withProbability p = fromList [(False, 1 - p'), (True, p')]
  where
    p' = fromRational $ max 0 $ min 1 $ toRational p


-- Transformation


-- | Applies a function to the values in the distribution.
--
--   >>> select abs $ uniform [-1, 0, 1]
--   fromList [(0,1 % 3),(1,2 % 3)]
select :: Ord b => (a -> b) -> Distribution a -> Distribution b
select f (Distribution xs) = Distribution $ Map.mapKeysWith (+) f xs

-- | Returns a new distribution conditioning on the predicate holding
--   on the value.
--
--   >>> assuming (> 2) $ uniform [1 .. 6]
--   fromList [(3,1 % 4),(4,1 % 4),(5,1 % 4),(6,1 % 4)]
--
--   Note that the resulting distribution will be invalid
--   if the predicate does not hold on any of the values.
--
--   >>> assuming (> 7) $ uniform [1 .. 6]
--   fromList []
assuming :: (a -> Bool) -> Distribution a -> Distribution a
assuming f (Distribution xs) = Distribution $ fmap adjust filtered
  where
    filtered = Map.filterWithKey (const . f) xs
    adjust x = x * (1 / total)
    total = sum $ Map.elems filtered


-- | Returns a new distribution using the Bayesian update rule.
--
-- Using this example:
-- https://en.wikipedia.org/wiki/Bayesian_inference#Probability_of_a_hypothesis
--
-- > data CookieBowl = Bowl1 | Bowl2 deriving (Eq,Ord)
-- > data CookieType = Plain | ChocolateChip deriving (Eq,Ord)
-- >
-- > assumption :: Distribution CookieBowl
-- > assumption = uniform [Bowl1,Bowl2]
-- >
-- > update :: Cookie -> Distribution CookieBowl -> Distribution CookieBowl
-- > update c = observing f where
-- >   f b = case b of
-- >     -- Bowl #1 contains 10 chocolate chip cookies and 30 plain cookies
-- >     Bowl1 -> fromList [(c == ChocolateChip,10),(c == Plain,30)]
-- >     -- Bowl #2 contains 20 of each flavour of cookie
-- >     Bowl2 -> fromList [(c == ChocolateChip,20),(c == Plain,20)]
--
-- The "update" function in this example can be used to update the probability
-- distribution of which bowl you have based on observing a random cookie inside
-- the bowl.
observing :: (a -> Distribution Bool) -> Distribution a -> Distribution a
observing f (Distribution xs) = Distribution $ fmap adjust filtered
  where
    filtered = Map.filter (/= 0) $ Map.mapWithKey tweak xs
    tweak x p = let
      Distribution px = f x
      pt = fromMaybe 0 $ Map.lookup True px
      in pt * p
    adjust x = x * (1 / total)
    total = sum $ Map.elems filtered

-- Combination

combineWith :: (Ord b) => (a -> a -> b) -> Distribution a -> Distribution a -> Distribution b
combineWith f (Distribution xs) (Distribution ys) = Distribution $ Map.unionsWith (+) $ do
    (x, p) <- Map.toList xs
    return $ Map.fromListWith (+) $ do
        (y, q) <- Map.toList ys
        return (f x y, p * q)

-- Sequences


-- | Binomial distribution.
--   Assigns to each number of successes its probability.
--
--   >>> trials 2 $ uniform [True, False]
--   fromList [(0,1 % 4),(1,1 % 2),(2,1 % 4)]
trials :: Int -> Distribution Bool -> Distribution Int
trials n d = Distribution $ Map.fromDistinctAscList $ if
    | p == 1    -> [(n, 1)]
    | p == 0    -> [(0, 1)]
    | otherwise -> zip outcomes probs
  where
    p = fromMaybe 0 $ Map.lookup True $ toMap d
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
--   >>> times 2 $ uniform [1 .. 3]
--   fromList [(2,1 % 9),(3,2 % 9),(4,1 % 3),(5,2 % 9),(6,1 % 9)]
--
--   This function makes use of the more efficient @trials@ functions
--   for input distributions of size @2@.
--
--   >>> size $ times 10000 $ uniform [1, 10]
--   10001
times :: (Num a, Ord a) => Int -> Distribution a -> Distribution a
n `times` d
    | s == 0 = d
    | n <= 0 = always 0
    | s == 1 = select (* n') d
    | s == 2 = case toList d of  -- Performs Bernoulli trials. (efficiency)
        [(a, p), (b, q)] -> select (go a b) $ trials n $ withProbability p
        _ -> error "times: size seems not to be properly defined."
    | otherwise = mult n
  where
    s = Map.size $ toMap d
    n' = fromInteger $ toInteger n
    go a b k = k' * a + (n' - k') * b
      where
        k' = fromInteger $ toInteger k

    -- Computes @k `times` d@ using a divide and conquer approach.
    mult 1 = d
    mult k = if r == 0 then twice d' else twice d' + d
      where
        d' = mult k'
        (k', r) = k `quotRem` 2

    -- Computes @d + d@ more efficiently.
    twice (Distribution xs) = Distribution $ Map.unionsWith (+) $ do
        (x, p) : ys <- init $ tails $ Map.toAscList xs
        return $ Map.fromDistinctAscList $ (:) (x + x, p * p) $ do
            (y, q) <- ys
            let p' = 2 * p * q
            return (y + x, p')

iid :: (Ord a) => (a -> a -> a) -> Int -> Distribution a -> Distribution a
iid f n d
    | n <= 0 = error "Called iid with a non-positive number of trials."
    | otherwise = go n
    where
      go 1 = d
      go m =
        let (i, j) = quotRem m 2 
            sub = go i
            combined = combineWith f sub sub
        in if j == 0
          then combined
          else combineWith f combined d


-- | Computes for each value in the distribution a new distribution, and then
--   combines those distributions, giving each the weight of the original value.
--
--   >>> uniform [1 .. 3] `andThen` (\ n -> uniform [1 .. n])
--   fromList [(1,11 % 18),(2,5 % 18),(3,1 % 9)]
--
--   See the 'Experiment' data type in the 'Data.Distribution.Monadic' module
--   for a more "natural" monadic interface. 
infixl 7 `andThen`
andThen :: Ord b => Distribution a -> (a -> Distribution b) -> Distribution b
andThen (Distribution xs) f = Distribution $
    Map.unionsWith (+) $ fmap go $ Map.toList xs
  where
    go (x, p) = fmap (* p) $ toMap $ f x


-- | Determines if a distribution is valid.
--
--   A distribution is valid if and only if its domain is non-empty.
--   Invalid distributions may arise from the use of 'assuming' for instance.
isValid :: Distribution a -> Bool
isValid (Distribution xs) = not $ Map.null xs
