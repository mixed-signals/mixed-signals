module Numeric.Layer.Reshape where
import Prelude(Show(..),(.),showString)

import Control.Applicative
import Numeric.Layer
import Numeric.Randomized
import Dependent.Size
import Numeric.Vector.Sized(reshape)

newtype Reshape a b = Reshape (a,b)

-- | Layer chaining with reshape of data between
-- the adjacent layers
(~~>) :: a -> b -> Reshape a b
a ~~> b = Reshape (a, b)

type a ~~> b = Reshape a b


instance (Show a, Show b) => Show (Reshape a b) where
  showsPrec n (Reshape (a,b)) = showsPrec n a . showString " ~~> " . showsPrec n b

instance (Randomized a, Randomized b) => Randomized (Reshape a b) where
  randomized = (\a b -> Reshape (a,b)) <$> randomized <*> randomized

instance (Layer a, Layer b, Volume (Outputs a) ~ Volume (Inputs b), ShapeSize (Outputs a), ShapeSize (Inputs b)) => Layer (Reshape a b) where
  type Inputs (Reshape a b) = Inputs a
  type Outputs (Reshape a b) = Outputs b
  type Tape (Reshape a b) = (Tape a, Tape b)
  type Gradient (Reshape a b) = (Gradient a, Gradient b)
  forward (Reshape (a,b)) x = (z, (tape_a,tape_b))
    where
      (y,tape_a) = forward a x
      (z, tape_b) = forward b (reshape y)
  backward (Reshape (a,b)) (tape_a, tape_b) dz = ((grad_a, grad_b), dx)
    where
      (grad_b, dy) = backward b tape_b dz
      (grad_a, dx) = backward a tape_a (reshape dy)
  applyGradient (Reshape (a,b)) (grad_a,grad_b) = Reshape (applyGradient a grad_a, applyGradient b grad_b)
