module SSRS.Fold where

import Prelude

import Control.Comonad.Cofree (Cofree, head, mkCofree, (:<))
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM2)
import Data.Either (Either(..))
import Data.Functor.Mu (Mu(..))
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..), fst, snd)
import Dissect.Class (class Dissect, right)
import SSRS.Algebra (Algebra, AlgebraM, GAlgebra, GAlgebraM)

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

cataM ∷ ∀ m p q v. MonadRec m ⇒ Dissect p q ⇒ AlgebraM m p v → Mu p → m v
cataM algebraM (In pt) = tailRecM2 go (right (Left pt)) Nil
  where
  go index stack =
    case index of
      Left (Tuple (In pt') pd) →
        pure (Loop { a: right (Left pt'), b: (pd : stack) })
      Right pv →
        case stack of
          (pd : stk) → do
            pv' ← algebraM pv
            pure (Loop { a: right (Right (Tuple pd pv')), b: stk })
          Nil → do
            Done <$> algebraM pv

prepro ∷ ∀ p q v. Dissect p q ⇒ (p ~> p) → Algebra p v → Mu p → v
prepro pre algebra = cata (algebra <<< pre)

para ∷ ∀ p q v. Dissect p q ⇒ GAlgebra (Tuple (Mu p)) p v → Mu p → v
para gAlgebra = snd <<< cata algebra
  where
  algebra ∷ p (Tuple (Mu p) v) → Tuple (Mu p) v
  algebra n = Tuple (In (map fst n)) (gAlgebra n)

paraM ∷ ∀ m p q v. MonadRec m ⇒ Dissect p q ⇒ GAlgebraM (Tuple (Mu p)) m p v → Mu p → m v
paraM gAlgebraM = map snd <<< cataM algebraM
  where
  algebraM ∷ p (Tuple (Mu p) v) → m (Tuple (Mu p) v)
  algebraM n = Tuple (In (map fst n)) <$> gAlgebraM n

histo ∷ ∀ p q v. Dissect p q ⇒ GAlgebra (Cofree p) p v → Mu p → v
histo gAlgebra = head <<< cata algebra
  where
  algebra ∷ p (Cofree p v) → Cofree p v
  algebra n = gAlgebra n :< n

histoM :: forall m p q v. MonadRec m => Dissect p q => GAlgebraM (Cofree p) m p v -> Mu p -> m v
histoM gAlgebraM = map head <<< cataM algebraM
  where
  algebraM :: p (Cofree p v) -> m (Cofree p v)
  algebraM n = mkCofree <$> gAlgebraM n <*> pure n
