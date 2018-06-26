module Numeric.Randomized where

import Control.Monad.Random

class Randomized r where
  randomized :: MonadRandom m => m r
