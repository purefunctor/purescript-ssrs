module Benchmark.Folding.SSRSVR where

import Prelude

import Benchmark.Common (VRList, VRListF)
import Data.Functor.Polynomial (Const(..), Id(..))
import Record.Polynomial (to)
import SSRS.Fold (cata)
import Variant.Polynomial (match)

sigma ∷ VRList Int → Int
sigma = cata go
  where
  go :: VRListF Int Int -> Int
  go = match
    { nil: \_ -> 0
    , cons: to >>> \{ head: Const head, tail: Id tail } -> head + tail
    }
