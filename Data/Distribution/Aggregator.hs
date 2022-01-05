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


-- | Module containing functions to apply on
--   lists of values tagged with their probability,
--   in order to somehow aggregate or transform the
--   probabilities.
module Data.Distribution.Aggregator
    ( -- * Aggregation
      Aggregator
      -- ** Creation
    , makeAggregator
    , makePureAggregator
    , separated
      -- ** Application
    , modifyProbabilities
    , aggregateWith
      -- ** Useful aggregators
    , cumulative
    , decreasing
    , complementary
    ) where

import Data.Monoid

import Data.Distribution.Core


-- Aggregation


-- | Functions that can modify probabilities.
newtype Aggregator a = Aggregator
    { modifyProbabilities :: [(a, Probability)] -> [Probability]
      -- ^ Applies the aggregator and returns the modified list
      --   of probabilities.
    }

-- | Compose aggregators with `(<>)`
instance Semigroup (Aggregator a) where
    (Aggregator f) <> g = Aggregator (f . aggregateWith g)

-- | 'mempty' is the aggregator that leaves probabilities untouched
instance Monoid (Aggregator a) where
    mempty = Aggregator (map snd)
    mappend = (<>)

-- | Applies an aggregator on a list of values tagged with their probability.
--   The values themselves are left unchanged.
aggregateWith :: Aggregator a -> [(a, Probability)] -> [(a, Probability)]
aggregateWith (Aggregator f) xs = zip vs $ f xs
  where
    vs = map fst xs

-- | Creates an aggregator from a function ignoring the values.
--   The function should not modify the number of elements.
makePureAggregator :: ([Probability] -> [Probability]) -> Aggregator a
makePureAggregator f = Aggregator $ f . map snd

-- | Creates an aggregator from a function.
--   The function should not modify the number of elements.
makeAggregator :: ([(a, Probability)] -> [Probability]) -> Aggregator a
makeAggregator = Aggregator

-- | Aggregator that applies the first aggregator on values less than @x@
--   and the second on values greater than @x@. Potential probability at @x@
--   is left untouched.
separated :: Ord a => a -> Aggregator a -> Aggregator a -> Aggregator a
separated x la ga = makeAggregator go
  where
    go xs = mconcat
        [ modifyProbabilities la ls
        , modifyProbabilities mempty es
        , modifyProbabilities ga gs ]
      where
        (ls, egs) = span ((< x) . fst) xs
        (es, gs) = span ((== x) . fst) egs

-- | Adds to each probability the sum of the probabilities earlier in the list.
--
--   >>> aggregateWith cumulative $ toList $ uniform [1 .. 5]
--   [(1,1 % 5),(2,2 % 5),(3,3 % 5),(4,4 % 5),(5,1 % 1)]
cumulative :: Aggregator a
cumulative = makePureAggregator (scanl1 (+))

-- | Replaces each probability by its complement.
--
--   >>> aggregateWith complementary $ toList $ uniform [1 .. 5]
--   [(1,4 % 5),(2,4 % 5),(3,4 % 5),(4,4 % 5),(5,4 % 5)]
complementary :: Aggregator a
complementary = makePureAggregator (map (1 -))

-- | Adds to each probability the sum of probabilities later in the list.
--
--   >>> aggregateWith decreasing  $ toList $ uniform [1 .. 5]
--   [(1,1 % 1),(2,4 % 5),(3,3 % 5),(4,2 % 5),(5,1 % 5)]
decreasing :: Aggregator a
decreasing = makePureAggregator (scanr1 (+))
