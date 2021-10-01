module SSRS.Fold where

import Data.Either (Either(..))
import Data.Functor.Mu (Mu(..))
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))
import Dissect.Class (class Dissect, right)
import SSRS.Algebra (Algebra)

cata ∷ ∀ p q v. Dissect p q ⇒ Algebra p v → Mu p → v
cata algebra (In pt) = go (right (Left pt)) Nil
  where
  go index stack =
    case index of
      Left (Tuple (In pt') pd) →
        go (right (Left pt')) (pd : stack)
      Right pv →
        case stack of
          (pd : stk) →
            go (right (Right (Tuple pd (algebra pv)))) stk
          Nil →
            algebra pv
