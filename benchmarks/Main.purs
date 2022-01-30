module Benchmark.Main where

import Prelude

import Benchmark.Common (listOf, polyListOf, treeOf, vrListOf)
import Benchmark.Folding.SSRS as FoldingSSRS
import Benchmark.Folding.SSRSPoly as FoldingSSRSPoly
import Benchmark.Folding.SSRSVR as FoldingSSRSVR
import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Data.Array ((..))
import Effect (Effect)
import Test.QuickCheck.Arbitrary (arbitrary)

foldingList ∷ Benchmark
foldingList = mkBenchmark
  { slug: "foldingList"
  , title: "Integer summation"
  , sizes: (1 .. 25) <#> (_ * 100)
  , sizeInterpretation: "List length"
  , inputsPerSize: 10
  , gen: \n → listOf n arbitrary
  , functions:
      [ benchFn "ssrs" FoldingSSRS.sigma
      -- , benchFn "matryoshka" FoldingMatryoshka.sigma
      ]
  }

foldingListPoly ∷ Benchmark
foldingListPoly = mkBenchmark
  { slug: "foldingListPoly"
  , title: "Integer summation"
  , sizes: (1 .. 25) <#> (_ * 100)
  , sizeInterpretation: "List length"
  , inputsPerSize: 10
  , gen: \n → polyListOf n arbitrary
  , functions:
      [ benchFn "ssrs" FoldingSSRSPoly.sigma
      ]
  }

foldingListVr ∷ Benchmark
foldingListVr = mkBenchmark
  { slug: "foldingListVr"
  , title: "Integer summation"
  , sizes: (1 .. 25) <#> (_ * 100)
  , sizeInterpretation: "List length"
  , inputsPerSize: 10
  , gen: \n → vrListOf n arbitrary
  , functions:
      [ benchFn "ssrs" FoldingSSRSVR.sigma
      ]
  }

foldingTree ∷ Benchmark
foldingTree = mkBenchmark
  { slug: "foldingTree"
  , title: "Integer summation"
  , sizes: (1 .. 10) <#> (_ * 1)
  , sizeInterpretation: "Approximate Depth"
  , inputsPerSize: 10
  , gen: \n → treeOf n arbitrary
  , functions:
      [ benchFn "ssrs" FoldingSSRS.combine
      ]
  }

main ∷ Effect Unit
main = runSuite [ foldingList, foldingListPoly, foldingListVr, foldingTree ]
