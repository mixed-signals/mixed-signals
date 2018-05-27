{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Numeric.Layer where

import Dependent.Size
import Numeric.Vector.Sized

class (ShapeSize (Inputs layer), ShapeSize (Outputs layer)) => Layer layer where
  type Inputs layer :: Size
  type Outputs layer :: Size
  forward :: layer -> Sized (Inputs layer) -> Sized (Outputs layer)
