-- Copyright 2014 Romain Edelmann. All rights reserved.

-- | This modules provides distributions from coins and
--   functions on coins.
module Data.Distribution.Domain.Coin
    ( -- * Coin
      Coin
    , CoinSide (..)
    , coin
      -- ** Operations
    , flipsOf
    , reflipOn
    ) where

import Data.Distribution.Core

-- | Distribution over the sides of a coin.
type Coin = Distribution CoinSide

-- | Possible outcomes of a coin flip.
data CoinSide = Head | Tail
  deriving (Eq, Ord, Show, Read, Enum)

-- | Fair coin.
coin :: Coin
coin = uniform [Head, Tail]

-- | Flips `n` times the given coin and counts the number of heads.
flipsOf :: Int -> Coin -> Distribution Int
n `flipsOf` d = n `trials` select (== Head) d

-- | Rerolls the coin once if the first outcome satifies the given predicate.
reflipOn :: CoinSide -> Coin -> Coin
reflipOn s d = d `andThen` \ r -> if r == s then d else always r
