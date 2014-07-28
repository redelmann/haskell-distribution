-- Copyright 2014 Romain Edelmann. All rights reserved.

-- | This modules provides distributions from dice and
--   functions on dice.
module Data.Distribution.Dice
    ( -- * Dice
      dice
      -- ** Common dice
    , d4
    , d6
    , d8
    , d10
    , d20
      -- ** Operations
    , rollsOf
    , rerollOn ) where

import Data.Distribution

-- | Fair dice of @n@ faces.
dice :: Int -> Distribution Int
dice n = uniform [1 .. n]

-- | Fair dice of @4@ faces.
d4 :: Distribution Int
d4 = dice 4

-- | Fair dice of @6@ faces.
d6 :: Distribution Int
d6 = dice 6

-- | Fair dice of @8@ faces.
d8 :: Distribution Int
d8 = dice 8

-- | Fair dice of @10@ faces.
d10 :: Distribution Int
d10 = dice 10

-- | Fair dice of @12@ faces.
d12 :: Distribution Int
d12 = dice 12

-- | Fair dice of @20@ faces.
d20 :: Distribution Int
d20 = dice 20

-- | Rolls `n` times the given dice and sums the results.
rollsOf :: Int -> Distribution Int -> Distribution Int
n `rollsOf` d = n `times` d

-- | Rerolls the dice once if the first outcome satifies the given predicate.
rerollOn :: (Int -> Bool) -> Distribution Int -> Distribution Int
rerollOn f d = d `andThen` \ n -> if f n then d else always n
