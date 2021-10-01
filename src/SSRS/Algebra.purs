module SSRS.Algebra where

type Algebra f a = f a → a

type GAlgebra w f a = f (w a) → a
