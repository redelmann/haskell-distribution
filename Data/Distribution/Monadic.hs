{-# LANGUAGE GADTs #-}

{- Copyright 2016 Romain Edelmann

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. -}

-- | This modules provides a monadic interface to build distributions.
module Data.Distribution.Monadic 
    ( Experiment, from, run ) where

import Data.Distribution.Core

-- | Monadic description of distributions.
data Experiment a where
  Return :: a -> Experiment a
  Bind :: Experiment b -> (b -> Experiment a) -> Experiment a
  Prim :: (Ord a) => Distribution a -> Experiment a

instance Functor Experiment where
  fmap f d = Bind d (\ x -> Return (f x))

instance Applicative Experiment where
  pure x = Return x
  df <*> d = Bind df (\ f -> Bind d (\ x -> Return (f x)))

instance Monad Experiment where
  return x = Return x
  d >>= f = Bind d f

-- | Converts a concrete distribution into its
--   monadic representation.
from :: (Ord a) => Distribution a -> Experiment a
from d = Prim d

-- | Converts the monadic description of the distribution 
--   to a concrete distribution.
run :: (Ord a) => Experiment a -> Distribution a
run (Return x) = always x
run (Bind (Return x) f) = run (f x)
run (Bind (Bind i g) f) = run (Bind i (\ x -> Bind (g x) f))
run (Bind (Prim d) f) = d `andThen` \ x -> run (f x)
run (Prim d) = d