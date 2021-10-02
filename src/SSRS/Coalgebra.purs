module SSRS.Coalgebra where

type Coalgebra f a = a → f a

type CoalgebraM m f a = a → m (f a)

type GCoalgebra n f a = a → f (n a)
