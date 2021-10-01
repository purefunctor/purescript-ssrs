module SSRS.Unfold where

import Data.Either (Either(..))
import Data.Functor.Mu (Mu(..))
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))
import Dissect.Class (class Dissect, right)
import SSRS.Coalgebra (Coalgebra)

ana ∷ ∀ p q v. Dissect p q ⇒ Coalgebra p v → v → Mu p
ana coalgebra seed = go (right (Left (coalgebra seed))) Nil
  where
  go index stack =
    case index of
      Left (Tuple pt pd) →
        go (right (Left (coalgebra pt))) (pd : stack)
      Right pv →
        case stack of
          (pd : stk) →
            go (right (Right (Tuple pd (In pv)))) stk
          Nil →
            In pv
