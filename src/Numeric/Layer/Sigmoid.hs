module Numeric.Layer.Sigmoid where
import Numeric.Layer
import Dependent.Size
import Numeric.Vector.Sized
import qualified Numeric.Vector.Sized as Sized
import GHC.TypeLits(KnownNat,Nat)

newtype Sigmoid (n :: Nat) = Sigmoid ()

sigmoid :: Floating x => x -> x
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Num a => a -> a
sigmoid' y = (1 - y) * y

instance KnownNat n => Layer (Sigmoid n) where
  type Inputs (Sigmoid n) = ZZ ::. n
  type Outputs (Sigmoid n) = ZZ ::. n
  type Tape (Sigmoid n) = SizedArray (ZZ ::. n)
  type Gradient (Sigmoid n) = ()
  forward _ x = (y, tape)
    where
      y = Sized.map sigmoid x
      tape = y
  backward _ tape dy = (gradient, dx)
    where
      gradient = ()
      dx = Sized.zipWith (*) dy (Sized.map sigmoid' tape)

