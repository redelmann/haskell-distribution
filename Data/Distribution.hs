
-- | This module defines finite discrete probability distributions
--   and efficient ways to construct, modify, combine, measure and sample them.
--
--   The various functionalities are each defined in their own submodules and
--   simply re-exported here. Note that not all modules in the package are
--   directly exported by this top-level module.
module Data.Distribution
    ( -- * Exported modules

      module Data.Distribution.Core
      -- | The "Data.Distribution.Core" module contains the definition of
      --   finite discrete probability distributions, as well as functions for
      --   constructing, deconstructing and combining such distributions.

    , module Data.Distribution.Measure
      -- | The "Data.Distribution.Measure" module contains functions to measure
      --   the 'probability' of events, the 'expectation' and 'variance' of
      --   numeric distributions as well as functions for getting interesting
      --   values, such as 'median', 'quantile's and 'modes', out of
      --   distributions.

    , module Data.Distribution.Aggregator
      -- | The "Data.Distribution.Aggregator" module provides way to modify
      --   the probabilities of lists of values tagged with their probability.
      --   The module also defines common aggregators such as 'complementary'
      --   or 'cumulative'.

    , module Data.Distribution.Sample
      -- | The "Data.Distribution.Sample" module provides ways to efficiently
      --   randomly sample values from distributions.

      -- * Other modules

      -- | Not all modules related to distributions are exported by default.
      --   Here is a list of such modules:
      --
      --       * "Data.Distribution.Plot" : For plotting distributions.
      --
      --       * "Data.Distribution.Domain.Dice" : Distributions over dice.
      --
      --       * "Data.Distribution.Domain.Coin" : Distributions over coins.

      -- * Usage

      -- ** Basics

      -- | To illustrate how to use the module, let's walk through some simple
      --   examples. Let us first create a uniform distribution over the
      --   integers from 1 to 4.
      --
      --   >>> uniform [1 .. 4]
      --   fromList [(1,1 % 4),(2,1 % 4),(3,1 % 4),(4,1 % 4)]
      --
      --   The textual representation of distributions lists every value in the
      --   distribution with non-zero probability, in ascending order. To each
      --   value is associated its probability in the distribution.
      --
      --   We can use the 'probability' function to get the probability of some
      --   predicate in the distribution.
      --
      --   >>> probability (>= 2) $ uniform [1 .. 4]
      --   3 % 4
      --   >>> probability (< 1) $ uniform [1 .. 4]
      --   0 % 1

      -- ** Measures

      -- | We can also compute some other measures on the distributions, such
      --   as for instance 'expectation' and 'variance'.
      --   (For more details, see "Data.Distribution.Measure")
      --
      --   >>> expectation $ uniform [1 .. 4]
      --   2.5
      --   >>> variance $ uniform [1 .. 4]
      --   1.25

      -- ** Transforming distributions

      -- | Distributions can be transformed and combined in various ways.
      --   For instance, to apply a function on the values in a distribution
      --   'select' can be used.
      --   (For more details, see "Data.Distribution.Core")
      --
      --   >>> select (\ x -> x * x) $ uniform [-2, 0, 2]
      --   fromList [(0,1 % 3),(4,2 % 3)]
      --   >>> select (> 3) $ uniform [1 .. 10]
      --   fromList [(False,3 % 10),(True,7 % 10)]
      --
      --   The 'andThen' function can be used to create distributions
      --   that result from first taking a value from a distribution,
      --   and then, depending on that value, returning a new distribution.
      --
      --   >>> uniform [1 .. 2] `andThen` (\ n -> uniform [-n .. n])
      --   fromList [(-2,1 % 10),(-1,4 % 15),(0,4 % 15),(1,4 % 15),(2,1 % 10)]

      -- *** Numeric distributions

      -- | Distributions over numeric values can also be combined using
      --   addition, substraction, multiplication and division.
      --
      --   >>> uniform [1 .. 4] + uniform [1 .. 2]
      --   fromList [(2,1 % 8),(3,1 % 4),(4,1 % 4),(5,1 % 4),(6,1 % 8)]
      --   >>> uniform [1 .. 4] * uniform [0 .. 1]
      --   fromList [(0,1 % 2),(1,1 % 8),(2,1 % 8),(3,1 % 8),(4,1 % 8)]
      --
      --   For multiple experiments, the 'trials' and 'times' functions
      --   can be used. @trials@ counts the number of successes from
      --   @n@ random independant experiments.
      --
      --   >>> trials 4 $ uniform [True, False]
      --   fromList [(0,1 % 16),(1,1 % 4),(2,3 % 8),(3,1 % 4),(4,1 % 16)]
      --   >>> trials 2 $ withProbability 0.75
      --   fromList [(0,1 % 16),(1,3 % 8),(2,9 % 16)]
      --
      --   On the other hand, 'times' sums the outcome of @n@
      --   independant experiments.
      --
      --   >>> times 2 $ uniform [1, 2, 3]
      --   fromList [(2,1 % 9),(3,2 % 9),(4,1 % 3),(5,2 % 9),(6,1 % 9)]
      --
      --   Note the difference between @*@ and 'times'.
      --
      --   >>> times 2 $ uniform [1 .. 2]
      --   fromList [(2,1 % 4),(3,1 % 2),(4,1 % 4)]
      --   >>> 2 * uniform [1 .. 2]
      --   fromList [(2,1 % 2),(4,1 % 2)]

      -- ** Sampling

      -- | To get random values from distributions, we must first create
      --   a generator. For this, the 'fromDistribution' function is used.
      --   Once we have a generator, we can get random values using the
      --   'sample' or 'getSample' functions. 'sample' takes a generator
      --   and a 'StdGen' from "System.Random" and returns a random value
      --   from the distribution and a new 'StdGen'.
      --
      --   >>> let g = fromDistribution $ trials 10 $ withProbability 0.75
      --   >>> fst $ sample g $ mkStdGen 12345
      --   7
      --   >>> fst $ sample g $ mkStdGen 67890
      --   9
      --
      --   On the other hand, 'getSample' does the same, but directly in a
      --   'MonadRandom' from "Control.Monad.Random".
      --   (See "Data.Distribution.Sample" for more details)

      -- ** Plotting

      -- | Have a look at the "Data.Distribution.Plot" if you are interested
      --   in plotting distributions to files.
    ) where

import System.Random (mkStdGen)  -- For doctest.

import Data.Distribution.Core
import Data.Distribution.Measure
import Data.Distribution.Aggregator
import Data.Distribution.Sample
