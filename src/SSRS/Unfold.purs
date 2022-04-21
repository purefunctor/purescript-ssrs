module SSRS.Unfold where

import Prelude

import Control.Monad.Free (Free, resume)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM2)
import Data.Either (Either(..), either)
import Data.Functor.Mu (Mu(..))
import Data.List (List(..), (:))
import Data.Variant as Variant
import Dissect.Class (class Dissect, Result(..), init, next)
import SSRS.Coalgebra (Coalgebra, CoalgebraM, GCoalgebra, GCoalgebraM)
import SSRS.Transform (Transform, TransformM)
import Safe.Coerce (class Coercible, coerce)

foreign import unsafeAna
  ∷ ∀ p q v
  . (p (Mu p) → Result p q v (Mu p))
  → (q v (Mu p) → v → Result p q v (Mu p))
  → Coalgebra p v
  → v
  → Mu p

ana ∷ ∀ p q v. Dissect p q ⇒ Coalgebra p v → v → Mu p
ana =
  let
    init' ∷ p (Mu p) → Result p q v (Mu p)
    init' = init

    next' ∷ q v (Mu p) → v → Result p q v (Mu p)
    next' = next
  in
    unsafeAna init' next'

anaM ∷ ∀ m p q v. MonadRec m ⇒ Dissect p q ⇒ CoalgebraM m p v → v → m (Mu p)
anaM coalgebraM seed = do
  start ← coalgebraM seed
  tailRecM2 go (init start) Nil
  where
  go ∷ Result p q (Mu p) v → List (q (Mu p) v) → m (Step _ (Mu p))
  go (Result index) stack = index # Variant.match
    { yield: \{ j: pt, qcj: pd } → do
        pt' ← coalgebraM pt
        pure (Loop { a: init pt', b: pd : stack })
    , return: \pv →
        case stack of
          pd : stk →
            pure (Loop { a: next pd (In pv), b: stk })
          Nil →
            pure (Done (In pv))
    }

transAna
  ∷ ∀ p p' q q'
  . Dissect p p'
  ⇒ Dissect q q'
  ⇒ Transform (Mu p) p q -- p (Mu p) -> q (Mu p)
  → Mu p
  → Mu q
transAna c = ana (coerce c ∷ Mu p → q (Mu p))

transAnaM
  ∷ ∀ m p p' q q'
  . MonadRec m
  ⇒ Dissect p p'
  ⇒ Dissect q q'
  ⇒ TransformM m (Mu p) p q -- p (Mu p) -> m (q (Mu p))
  → Mu p
  → m (Mu q)
transAnaM c = anaM (coerce c ∷ Mu p → m (q (Mu p)))

transAnaT
  ∷ ∀ p q
  . Dissect p q
  ⇒ (Mu p → Mu p)
  → Mu p
  → Mu p
transAnaT c = ana (coerce c ∷ Mu p → p (Mu p))

transAnaTM
  ∷ ∀ m p q
  . MonadRec m
  ⇒ Coercible (m (Mu p)) (m (p (Mu p)))
  ⇒ Dissect p q
  ⇒ (Mu p → m (Mu p))
  → Mu p
  → m (Mu p)
transAnaTM t = anaM (coerce t ∷ Mu p → m (p (Mu p)))

postpro ∷ ∀ p q v. Dissect p q ⇒ (p ~> p) → Coalgebra p v → v → Mu p
postpro post coalgebra = ana (post <<< coalgebra)

apo ∷ ∀ p q v. Dissect p q ⇒ GCoalgebra (Either (Mu p)) p v → v → Mu p
apo gCoalgebra = ana coalgebra <<< Right
  where
  coalgebra ∷ Either (Mu p) v → p (Either (Mu p) v)
  coalgebra = either (\(In n) → map Left n) gCoalgebra

apoM ∷ ∀ m p q v. MonadRec m ⇒ Dissect p q ⇒ GCoalgebraM (Either (Mu p)) m p v → v → m (Mu p)
apoM gCoalgebraM = anaM coalgebraM <<< Right
  where
  coalgebraM ∷ Either (Mu p) v → m (p (Either (Mu p) v))
  coalgebraM = either (\(In n) → pure (map Left n)) gCoalgebraM

futu ∷ ∀ p q v. Dissect p q ⇒ GCoalgebra (Free p) p v → v → Mu p
futu gCoalgebra = ana coalgebra <<< pure
  where
  coalgebra ∷ Free p v → p (Free p v)
  coalgebra = either identity gCoalgebra <<< resume

futuM ∷ ∀ m p q v. MonadRec m ⇒ Dissect p q ⇒ GCoalgebraM (Free p) m p v → v → m (Mu p)
futuM gCoalgebraM = anaM coalgebraM <<< pure
  where
  coalgebraM ∷ Free p v → m (p (Free p v))
  coalgebraM = either (pure <<< identity) gCoalgebraM <<< resume
