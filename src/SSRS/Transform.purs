module SSRS.Transform where

type Transform t f g = f t → g t

type TransformM m t f g = f t → m (g t)
