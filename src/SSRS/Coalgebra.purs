module SSRS.Coalgebra where

type Coalgebra f a = a → f a

type CoalgebraM ∷ (Type → Type) → (Type → Type) → Type → Type
type CoalgebraM m f a = a → m (f a)

type GCoalgebra ∷ (Type → Type) → (Type → Type) → Type → Type
type GCoalgebra n f a = a → f (n a)

type GCoalgebraM ∷ (Type → Type) → (Type → Type) → (Type → Type) → Type → Type
type GCoalgebraM n m f a = a → m (f (n a))
