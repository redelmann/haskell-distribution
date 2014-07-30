
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

      -- | For plotting distributions, see the "Data.Distribution.Plot" module,
      --   and for various domain specific distributions, see for instance
      --   "Data.Distribution.Domain.Dice" and "Data.Distribution.Domain.Coin".
    ) where

import Data.Distribution.Core
import Data.Distribution.Measure
import Data.Distribution.Aggregator
import Data.Distribution.Sample
