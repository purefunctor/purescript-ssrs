module SSRS.Refold where

import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))
import Dissect.Class (class Dissect, right)
import SSRS.Algebra (Algebra)
import SSRS.Coalgebra (Coalgebra)

hylo :: forall p q v w. Dissect p q => Algebra p v -> Coalgebra p w -> w -> v
hylo algebra coalgebra seed = go (right (Left (coalgebra seed))) Nil
  where
  go index stack =
    case index of
      Left (Tuple pt pd) ->
        go (right (Left (coalgebra pt))) (pd : stack)
      Right pv ->
        case stack of
          (pd : stk) ->
            go (right (Right (Tuple pd (algebra pv)))) stk
          Nil ->
            algebra pv
