module Numeric.Network where

import Data.Kind
import Dependent.Size
import Numeric.Layer
import Numeric.Vector.Sized

infixr 6 :~>

type family NetworkOutput (layers :: [Type]) where
  NetworkOutput '[x] = Outputs x
  NetworkOutput (x ': y ': rest) = NetworkOutput (y ': rest)

data Network (layers :: [Type]) where
  Last :: Layer x => x -> Network '[x]
  (:~>) :: (Layer x, Outputs x ~ Inputs y) => x -> Network (y:xs) -> Network (x : y : xs)

predict
  :: Network (x : xs) -> SizedArray (Inputs x) -> SizedArray (NetworkOutput (x : xs))
predict (Last layer ) x = fst (forward layer x)
predict (layer:~>net) x = predict net y where y = fst (forward layer x)

