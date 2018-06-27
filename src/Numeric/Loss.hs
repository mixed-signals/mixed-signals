module Numeric.Loss where

import Dependent.Size
import Numeric.Vector.Sized

class Loss (size :: Size) fn where
  lossDerivative :: fn -> SizedArray size -> SizedArray size -> SizedArray size
