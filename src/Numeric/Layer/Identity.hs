module Numeric.Layer.Identity where

import Numeric.Layer
import Numeric.Randomized
import Dependent.Size
newtype Identity (size :: Size) = Identity ()
  deriving(Show)

instance Randomized (Identity size) where
  randomized = pure (Identity ())

instance Layer (Identity size) where
  type Inputs (Identity size) = size
  type Outputs (Identity size) = size
  type Tape (Identity size) = ()
  type Gradient (Identity size) = ()
  forward _ x = (x, ())
  backward _ _ dy = ((), dy)
  applyGradient layer _ = layer
