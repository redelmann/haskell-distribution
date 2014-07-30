-- Copyright 2014 Romain Edelmann. All rights reserved.

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
      -- ** Useful aggregators
    , cumulative
    , decreasing
    , complementary
    ) where

import Data.Monoid

import Data.Distribution


-- Aggregation


-- | Functions that can modify probabilities.
newtype Aggregator a = Aggregator
    { modifyProbabilities :: [(a, Probability)] -> [Probability]
      -- ^ Applies the aggregator and returns the modified list
      --   of probabilities.
    }

instance Monoid (Aggregator a) where
    mempty = Aggregator (map snd)
    mappend (Aggregator f) g = Aggregator (f . aggregateWith g)

-- | Applies an aggregator on a list of values tagged with their probability.
--   The values themselves are unchanged.
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
cumulative :: Aggregator a
cumulative = makePureAggregator (scanl1 (+))

-- | Replaces each probability by its complement.
complementary :: Aggregator a
complementary = makePureAggregator (map (1 -))

-- | Adds to each probability the sum of probabilities later in the list.
decreasing :: Aggregator a
decreasing = makePureAggregator (scanr1 (+))


