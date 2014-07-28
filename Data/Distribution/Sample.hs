{-# LANGUAGE MultiWayIf #-}

-- | This modules provides ways to randomly and efficiently sample values
--   from distributions.
--
--   Internally, Walker's algorithm is used, so that values can be sampled
--   in constant time.
module Data.Distribution.Sample
    ( Generator
    , fromDistribution
    , safeFromDistribution
    , sample ) where

import Control.Monad.Random
import Control.Monad.ST
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

import Data.Distribution

-- | Generator of random values of type @a@.
--
--   Can be created in linear time from distributions
--   and sampled in constant time.
data Generator a = Generator
    { probabilities :: Vector Double
      -- ^ Probability to stay in the bucket.
    , values :: Vector a
      -- ^ Value in the bucket.
    , indexes :: Vector Int
      -- ^ Index of the "guest" value. Used when the bucket is left.
    } deriving (Show)

instance Functor Generator where
    fmap f (Generator ps vs is) = Generator ps (fmap f vs) is

-- | Creates a generator from the given non-empty distribution.
--
--   Runs in @O(n)@ time where @n@ is the size of the distribution.
fromDistribution :: Distribution a -> Generator a
fromDistribution d = case toList d of
    [] -> error "makeGenerator: Undefined on empty distributions."
    xs -> generate xs
  where
    n = size d

    -- Creates the generator using Walker's algorithm.
    --
    -- The main idea is to put each value into its own bucket.
    -- In addition, each bucket has a probability to be discarded when picked,
    -- in which case another value, the "guest" of the bucket,
    -- is chosen instead.
    --
    -- The following procedure sets the probabilities and guests of
    -- the bucket so that the probability to choose the value of a bucket
    -- is the probability of the value in the input distribution.
    generate xs = runST $ do
        -- The values are directly from the list.
        let vs = Vector.fromList as

        -- The probability to stay in the bucket is @n@ times the
        -- probability in the distribution. This is due to the fact
        -- that each of the @n@ buckets is chosen with probability @1 / n@.
        -- Note that this can well exceed @1@. This will be taken care
        -- during the equilibration phase.
        -- In case the value exceed @1@, the bucket is said to be overfilled,
        -- and if its is strictly less than @1@, underfilled.
        ps <- Vector.thaw $ Vector.fromList sqs

        -- The indexes of "guest" values.
        -- The correct indexes will be set during the equilibration phase.
        -- Guest values are used by underfilled buckets.
        is <- MVector.replicate n 0

        -- The 'go' function is used to equilibrate the buckets, by assigning
        -- unused space underfilled buckets to overfilled buckets.
        --
        -- As first argument are the indexes which have a probability > 1
        -- (indexes of overfilled buckets),
        -- and as second argument those which have a probability < 1
        -- (indexed of underfilled buckets)
        --
        -- The idea of the function is to take an overfilled and an underfilled
        -- bucket, and to completely "fill" the underfilled bucket.
        -- To do so, the overfilled bucket is registered as the guest of the
        -- underfilled bucket. The probability of the overfilled bucket is
        -- then reduced by the amount that was poured into the underfilled
        -- bucket.
        let go (o : os) (u : us) = do
                -- First, we register o as the guest of u.
                MVector.write is u o

                -- We then update the probability of o.
                po <- MVector.read ps o
                pu <- MVector.read ps u
                let po' = po - (1 - pu)
                MVector.write ps o po'

                -- We recurse on the new overfilled and underfilled buckets.
                if | po' < 1 -> go os (o : us)  -- We took too much from o.
                   | po' == 1 -> go os us       -- o perfectly fits its bucket.
                   | otherwise -> go (o : os) us  -- o is still too large.

            go [] [] = return ()  -- All buckets are filled.
            go _ _ = error "makeGenerator: Implementation error."

        -- We select the initial overfilled and underfilled buckets.
        let os = map fst $ filter ((> 1) . snd) iqs
            us = map fst $ filter ((< 1) . snd) iqs

        -- We perform the equilibration phase.
        go os us

        -- Each bucket is now completely filled. We freeze the result.
        fps <- Vector.freeze ps
        fis <- Vector.freeze is
        return $ Generator (fmap fromRational fps) vs fis
      where
        -- Separating the values from their probability.
        (as, qs) = unzip xs

        -- Scaling the probabilities by @n@. This is due to the fact that each
        -- of the @n@ buckets is uniformly chosen with probability @1 / n@.
        sqs = map (* fromIntegral n) qs

        -- Indexed and scaled probabilities.
        iqs = zip [0 ..] sqs

-- | Safe version of 'fromDistribution'. Returns @Nothing@ when the
--   given distribution is empty.
safeFromDistribution :: Distribution a -> Maybe (Generator a)
safeFromDistribution d = if size d == 0
    then Nothing
    else Just $ fromDistribution d

-- | Takes a random value from the generator.
--
--   Runs in constant @O(1)@ time.
sample :: MonadRandom m => Generator a -> m a
sample g = do
    let n = Vector.length $ values g
    u <- getRandom
    j <- getRandomR (0, n - 1)
    let i = if u < probabilities g ! j then j else indexes g ! j
    return $ values g ! i
