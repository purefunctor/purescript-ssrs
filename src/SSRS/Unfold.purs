module SSRS.Unfold where

import Prelude

import Control.Monad.Free (Free, resume)
import Data.Either (Either(..), either)
import Data.Functor.Mu (Mu(..))
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))
import Dissect.Class (class Dissect, right)
import SSRS.Coalgebra (Coalgebra, GCoalgebra)

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

postpro ∷ ∀ p q v. Dissect p q ⇒ (p ~> p) → Coalgebra p v → v → Mu p
postpro post coalgebra = ana (post <<< coalgebra)

apo ∷ ∀ p q v. Dissect p q ⇒ GCoalgebra (Either (Mu p)) p v → v → Mu p
apo gCoalgebra = ana coalgebra <<< Right
  where
  coalgebra ∷ Either (Mu p) v → p (Either (Mu p) v)
  coalgebra = either (\(In n) → map Left n) gCoalgebra

futu :: forall p q v. Dissect p q => GCoalgebra (Free p) p v -> v -> Mu p
futu gCoalgebra = ana coalgebra <<< pure
  where
  coalgebra :: Free p v -> p (Free p v)
  coalgebra = either identity gCoalgebra <<< resume
