{-# LANGUAGE StandaloneDeriving #-}
module Numeric.Network where

import Data.Kind
import Dependent.Size
import Numeric.Layer
import Numeric.Vector.Sized
import Numeric.Loss

infixr 0 :~>

type family NetworkOutput (layers :: [Type]) where
  NetworkOutput '[x] = Outputs x
  NetworkOutput (x ': y ': rest) = NetworkOutput (y ': rest)

type family NetworkInput (layers :: [Type]) where
  NetworkInput (x ': _) = Inputs x

data Network (layers :: [Type]) where
  Last :: Layer x => x -> Network '[x]
  (:~>) :: (Layer x, Outputs x ~ Inputs y) => x -> Network (y:xs) -> Network (x : y : xs)

instance (Show x) => Show (Network '[x]) where
  showsPrec n (Last g) = showsPrec n g
instance (Show x, Show (Network (y:xs))) => Show (Network (x:y:xs)) where
  showsPrec n (layer :~> net) = showsPrec n layer . showString " :~> " . showsPrec n net


infixr 0 :|>
data Tapes (layers :: [Type]) where
  LastTape :: Layer x => Tape x -> Tapes '[x]
  (:|>) :: (Layer x, Outputs x ~ Inputs y) => Tape x -> Tapes (y:xs) -> Tapes (x:y:xs)

instance (Show (Tape x)) => Show (Tapes '[x]) where
  showsPrec n (LastTape g) = showsPrec n g
instance (Show (Tape x), Show (Tapes (y:xs))) => Show (Tapes (x:y:xs)) where
  showsPrec n (tape :|> tapes) = showsPrec n tape . showString " :|> " . showsPrec n tapes

infixr 0 :<|
data Gradients (layers :: [Type]) where
  LastGradient :: Layer x => Gradient x -> Gradients '[x]
  (:<|) :: (Layer x, Outputs x ~ Inputs y) => Gradient x -> Gradients (y:xs) -> Gradients (x : y : xs)

instance (Show (Gradient x)) => Show (Gradients '[x]) where
  showsPrec n (LastGradient g) = showsPrec n g
instance (Show (Gradient x), Show (Gradients (y:xs))) => Show (Gradients (x:y:xs)) where
  showsPrec n (grad :<| grads) = showsPrec n grad . showString " :<| " . showsPrec n grads

predict
  :: Network layers
  -> SizedArray (NetworkInput layers)
  -> SizedArray (NetworkOutput layers)
predict (Last layer ) x = fst (forward layer x)
predict (layer:~>net) x = predict net y where y = fst (forward layer x)

predict'
  :: Network layers
  -> SizedArray (NetworkInput layers)
  -> (SizedArray (NetworkOutput layers), Tapes layers)
predict' (Last layer) x = let (y, tape) = forward layer x in (y, LastTape tape)
predict' (layer:~>net) x =
  let (y, tape) = forward layer x
  in  let (z, tapes) = predict' net y in (z, tape :|> tapes)

backprop
  :: Network layers
  -> Tapes layers
  -> SizedArray (NetworkOutput layers)
  -> (Gradients layers, SizedArray (NetworkInput layers))
backprop (Last layer) (LastTape tape) dy =
  let (grad, dx) = backward layer tape dy in (LastGradient grad, dx)
backprop (layer:~>net) (tape:|>tapes) dz =
  let (grads, dy) = backprop net tapes dz
  in  let (grad, dx) = backward layer tape dy in (grad :<| grads, dx)

backpropagation
  :: (Loss (NetworkOutput layers) loss)
  => Network layers
  -> loss
  -> SizedArray (NetworkInput layers)
  -> SizedArray (NetworkOutput layers)
  -> (Gradients layers, SizedArray (NetworkInput layers))
backpropagation net loss x target = backprop net tapes dy
  where
    (y, tapes) = predict' net x
    dy         = lossDerivative loss y target

learn :: Network layers -> Gradients layers -> Network layers
learn (Last layer ) (LastGradient grad) = Last (applyGradient layer grad)
learn (layer:~>net) (grad:<|grads     ) = applyGradient layer grad :~> learn net grads

train
  :: (Loss (NetworkOutput layers) loss)
  => Network layers
  -> loss
  -> SizedArray (NetworkInput layers)
  -> SizedArray (NetworkOutput layers)
  -> Network layers
train net loss x t = f $ backpropagation net loss x t
  where f (gradients, _) = learn net gradients
