module Benchmark.Folding.SSRSPoly where

import Prelude

import Benchmark.Common (ListP)
import Dissect.Generic (Const(..), Id(..), Product(..), Sum(..))
import Data.Functor.Mu (Mu)
import SSRS.Fold (cata)

sigma ∷ Mu (ListP Int) → Int
sigma = cata go
  where
  go (SumL (Const _)) = 0
  go (SumR (Product (Const a) (Id n))) = a + n
