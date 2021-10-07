module SSRS.Algebra where

type Algebra f a = f a → a

type AlgebraM ∷ (Type → Type) → (Type → Type) → Type → Type
type AlgebraM m f a = f a → m a

type GAlgebra ∷ (Type → Type) → (Type → Type) → Type → Type
type GAlgebra w f a = f (w a) → a

type GAlgebraM ∷ (Type → Type) → (Type → Type) → (Type → Type) → Type → Type
type GAlgebraM w m f a = f (w a) → m a
