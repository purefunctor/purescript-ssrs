module SSRS.Algebra where

type Algebra f a = f a → a

type AlgebraM m f a = f a → m a

type GAlgebra w f a = f (w a) → a

type GAlgebraM w m f a = f (w a) → m a
