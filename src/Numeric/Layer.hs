{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Numeric.Layer where

import Dependent.Size
import Numeric.Vector.Sized
import Data.Kind(Type)

class Layer layer where
  type Inputs layer :: Size
  type Outputs layer :: Size
  type Tape layer :: Type
  type Gradient layer :: Type

  forward :: layer
          -> SizedArray (Inputs layer)
          -> (SizedArray (Outputs layer), Tape layer)

  backward :: layer
           -> Tape layer
           -> SizedArray (Outputs layer)
           -> (Gradient layer, SizedArray (Inputs layer))

  applyGradient :: layer -> Gradient layer -> layer
