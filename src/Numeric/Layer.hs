{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Numeric.Layer where

import Dependent.Size
import Numeric.Vector.Sized
import Data.Kind(Type)
--
-- sigmoid x = 
--
-- sigmoid' y = y * (1 - y)

class (ShapeSize (Inputs layer), ShapeSize (Outputs layer)) => Layer layer where
  type Inputs layer :: Size
  type Outputs layer :: Size
  type Tape layer :: Type
  type Gradient layer :: Type

  forward :: layer
          -> Sized (Inputs layer)
          -> (Sized (Outputs layer), Tape layer)

  backward :: layer
           -> Tape layer
           -> Sized (Outputs layer)
           -> (Gradient layer, Sized (Inputs layer))

