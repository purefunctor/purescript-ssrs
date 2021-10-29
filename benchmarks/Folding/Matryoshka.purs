module Benchmark.Folding.Matryoshka where

import Prelude

import Benchmark.Common (ListF(..))
import Data.Functor.Mu (Mu)
import Matryoshka.Fold (cata)

sigma :: Mu (ListF Int) -> Int
sigma = cata go
  where
  go Nil = 0
  go (Cons a n) = a + n
