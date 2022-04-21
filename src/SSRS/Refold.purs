module SSRS.Refold where

import Prelude

import Control.Comonad.Cofree (Cofree, head, mkCofree, (:<))
import Control.Monad.Free (Free, resume)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM2)
import Data.Either (either)
import Data.Functor.Mu (Mu(..))
import Data.List (List(..), (:))
import Data.Variant as Variant
import Dissect.Class (class Dissect, Result(..), init, next)
import SSRS.Algebra (Algebra, AlgebraM, GAlgebra, GAlgebraM)
import SSRS.Coalgebra (Coalgebra, CoalgebraM, GCoalgebra, GCoalgebraM)
import SSRS.Transform (Transform, TransformM)
import Safe.Coerce (class Coercible, coerce)

foreign import unsafeHylo
  ∷ ∀ p q v w
  . (p (Mu p) → Result p q v (Mu p))
  → (q v (Mu p) → v → Result p q v (Mu p))
  → Algebra p v
  → Coalgebra p w
  → w
  → v

hylo ∷ ∀ p q v w. Dissect p q ⇒ Algebra p v → Coalgebra p w → w → v
hylo =
  let
    init' ∷ p (Mu p) → Result p q v (Mu p)
    init' = init

    next' ∷ q v (Mu p) → v → Result p q v (Mu p)
    next' = next
  in
    unsafeHylo init' next'

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
  tailRecM2 go (init start) Nil
  where
  go ∷ Result p q v w → List (q v w) → m (Step _ v)
  go (Result index) stack = index # Variant.match
    { yield: \{ j: pt, qcj: pd } → do
        pt' ← coalgebraM pt
        pure (Loop { a: init pt', b: pd : stack })
    , return: \pv →
        case stack of
          pd : stk → do
            pv' ← algebraM pv
            pure (Loop { a: next pd pv', b: stk })
          Nil →
            Done <$> algebraM pv
    }

transHylo
  ∷ ∀ p p' q q' r r'
  . Dissect p p'
  ⇒ Dissect q q'
  ⇒ Dissect r r'
  ⇒ Transform (Mu q) r q -- r (Mu q) -> q (Mu q)
  → Transform (Mu p) p r -- p (Mu p) -> r (Mu p)
  → Mu p
  → Mu q
transHylo a c =
  hylo
    (coerce a ∷ r (Mu q) → Mu q) -- q (Mu q) = Mu q
    (coerce c ∷ Mu p → r (Mu p)) -- p (Mu p) = Mu p

transHyloM
  ∷ ∀ m p p' q q' r r'
  . MonadRec m
  ⇒ Coercible (m (q (Mu q))) (m (Mu q))
  ⇒ Dissect p p'
  ⇒ Dissect q q'
  ⇒ Dissect r r'
  ⇒ TransformM m (Mu q) r q -- r (Mu q) -> m (q (Mu q))
  → TransformM m (Mu p) p r -- p (Mu p) -> m (r (Mu p))
  → Mu p
  → m (Mu q)
transHyloM a c =
  hyloM
    (coerce a ∷ r (Mu q) → m (Mu q))
    (coerce c ∷ Mu p → m (r (Mu p)))

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

dynaM
  ∷ ∀ m p q v w
  . MonadRec m
  ⇒ Dissect p q
  ⇒ GAlgebraM (Cofree p) m p w
  → CoalgebraM m p v
  → v
  → m w
dynaM gAlgebraM coalgebraM = map head <<< hyloM algebraM coalgebraM
  where
  algebraM ∷ p (Cofree p w) → m (Cofree p w)
  algebraM n = mkCofree <$> gAlgebraM n <*> pure n

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

codynaM
  ∷ ∀ m p q v w
  . MonadRec m
  ⇒ Dissect p q
  ⇒ AlgebraM m p w
  → GCoalgebraM (Free p) m p v
  → v
  → m w
codynaM algebraM gCoalgebraM = hyloM algebraM coalgebraM <<< pure
  where
  coalgebraM ∷ Free p v → m (p (Free p v))
  coalgebraM = either (pure <<< identity) gCoalgebraM <<< resume

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

chronoM
  ∷ ∀ m p q v w
  . MonadRec m
  ⇒ Dissect p q
  ⇒ GAlgebraM (Cofree p) m p w
  → GCoalgebraM (Free p) m p v
  → v
  → m w
chronoM gAlgebraM gCoalgebraM = map head <<< hyloM algebraM coalgebraM <<< pure
  where
  algebraM ∷ p (Cofree p w) → m (Cofree p w)
  algebraM n = mkCofree <$> gAlgebraM n <*> pure n

  coalgebraM ∷ Free p v → m (p (Free p v))
  coalgebraM = either (pure <<< identity) gCoalgebraM <<< resume
