
module Data.Distribution.Dice where

import Data.Distribution

d4 :: Distribution Int
d4 = uniform [1 .. 4]

d6 :: Distribution Int
d6 = uniform [1 .. 6]

d8 :: Distribution Int
d8 = uniform [1 .. 8]

d10 :: Distribution Int
d10 = uniform [1 .. 10]

d12 :: Distribution Int
d12 = uniform [1 .. 12]

d20 :: Distribution Int
d20 = uniform [1 .. 20]

rollsOf :: Int -> Distribution Int -> Distribution Int
n `rollsOf` d = n `times` d
