module SSRS.Transform where

type Transform ∷ Type → (Type → Type) → (Type → Type) → Type
type Transform t f g = f t → g t

type TransformM ∷ (Type → Type) → Type → (Type → Type) → (Type → Type) → Type
type TransformM m t f g = f t → m (g t)
