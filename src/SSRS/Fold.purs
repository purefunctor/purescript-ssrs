module SSRS.Fold where

import Prelude

import Control.Comonad.Cofree (Cofree, head, (:<))
import Data.Either (Either(..))
import Data.Functor.Mu (Mu(..))
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..), fst, snd)
import Dissect.Class (class Dissect, right)
import SSRS.Algebra (Algebra, GAlgebra)

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

prepro ∷ ∀ p q v. Dissect p q ⇒ (p ~> p) → Algebra p v → Mu p → v
prepro pre algebra = cata (algebra <<< pre)

para ∷ ∀ p q v. Dissect p q ⇒ GAlgebra (Tuple (Mu p)) p v → Mu p → v
para gAlgebra = snd <<< cata algebra
  where
  algebra ∷ p (Tuple (Mu p) v) → Tuple (Mu p) v
  algebra n = Tuple (In (map fst n)) (gAlgebra n)

histo ∷ ∀ p q v. Dissect p q ⇒ GAlgebra (Cofree p) p v → Mu p → v
histo gAlgebra = head <<< cata algebra
  where
  algebra ∷ p (Cofree p v) → Cofree p v
  algebra n = gAlgebra n :< n
