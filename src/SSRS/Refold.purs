module SSRS.Refold where

import Prelude

import Control.Monad.Free (Free, resume)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM2)
import Control.Comonad.Cofree (Cofree, head, (:<))
import Data.Either (Either(..), either)
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))
import Dissect.Class (class Dissect, right)
import SSRS.Algebra (Algebra, AlgebraM, GAlgebra)
import SSRS.Coalgebra (Coalgebra, CoalgebraM, GCoalgebra)

hylo ∷ ∀ p q v w. Dissect p q ⇒ Algebra p v → Coalgebra p w → w → v
hylo algebra coalgebra seed = go (right (Left (coalgebra seed))) Nil
  where
  go index stack =
    case index of
      Left (Tuple pt pd) →
        go (right (Left (coalgebra pt))) (pd : stack)
      Right pv →
        case stack of
          (pd : stk) →
            go (right (Right (Tuple pd (algebra pv)))) stk
          Nil →
            algebra pv

hyloM
  ∷ ∀ m p q v w
  . MonadRec m
  ⇒ Dissect p q
  ⇒ AlgebraM m p v
  → CoalgebraM m p w
  → w
  → m v
hyloM algebraM coalgebraM seed = do
  start ← coalgebraM seed
  tailRecM2 go (right (Left start)) Nil
  where
  go index stack =
    case index of
      Left (Tuple pt pd) → do
        next ← coalgebraM pt
        pure (Loop { a: right (Left next), b: (pd : stack) })
      Right pv →
        case stack of
          (pd : stk) → do
            next ← algebraM pv
            pure (Loop { a: right (Right (Tuple pd next)), b: stk })
          Nil → do
            Done <$> algebraM pv

dyna
  ∷ ∀ p q v w
  . Dissect p q
  ⇒ GAlgebra (Cofree p) p w
  → Coalgebra p v
  → v
  → w
dyna gAlgebra coalgebra = head <<< hylo algebra coalgebra
  where
  algebra ∷ p (Cofree p w) → Cofree p w
  algebra n = gAlgebra n :< n

codyna
  ∷ ∀ p q v w
  . Dissect p q
  ⇒ Algebra p w
  → GCoalgebra (Free p) p v
  → v
  → w
codyna algebra gCoalgebra = hylo algebra coalgebra <<< pure
  where
  coalgebra ∷ Free p v → p (Free p v)
  coalgebra = either identity gCoalgebra <<< resume

chrono
  ∷ ∀ p q v w
  . Dissect p q
  ⇒ GAlgebra (Cofree p) p w
  → GCoalgebra (Free p) p v
  → v
  → w
chrono gAlgebra gCoalgebra = head <<< hylo algebra coalgebra <<< pure
  where
  algebra ∷ p (Cofree p w) → Cofree p w
  algebra n = gAlgebra n :< n

  coalgebra ∷ Free p v → p (Free p v)
  coalgebra = either identity gCoalgebra <<< resume
