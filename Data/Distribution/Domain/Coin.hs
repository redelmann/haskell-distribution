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
