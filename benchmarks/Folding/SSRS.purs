module Benchmark.Folding.SSRS where

import Prelude

import Benchmark.Common (ListF(..), TreeF(..))
import Data.Functor.Mu (Mu)
import SSRS.Fold (cata)

sigma ∷ Mu (ListF Int) → Int
sigma = cata go
  where
  go Nil = 0
  go (Cons a n) = a + n

combine :: Mu (TreeF Int) -> Int
combine = cata go
  where
  go (Leaf a) = a
  go (Fork a b) = a + b
