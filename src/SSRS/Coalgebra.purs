module SSRS.Coalgebra where

type Coalgebra f a = a → f a

type GCoalgebra n f a = a → f (n a)
