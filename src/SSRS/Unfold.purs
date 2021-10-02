module SSRS.Unfold where

import Prelude

import Control.Monad.Free (Free, resume)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM2)
import Data.Either (Either(..), either)
import Data.Functor.Mu (Mu(..))
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))
import Dissect.Class (class Dissect, right)
import SSRS.Coalgebra (Coalgebra, CoalgebraM, GCoalgebra, GCoalgebraM)

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

anaM ∷ ∀ m p q v. MonadRec m ⇒ Dissect p q ⇒ CoalgebraM m p v → v → m (Mu p)
anaM coalgebraM seed = do
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
          (pd : stk) →
            pure (Loop { a: right (Right (Tuple pd (In pv))), b: stk })
          Nil →
            pure (Done (In pv))

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
