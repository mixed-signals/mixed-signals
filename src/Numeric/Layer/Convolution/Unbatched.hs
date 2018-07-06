{-# LANGUAGE UndecidableInstances #-}
module Numeric.Layer.Convolution.Unbatched where
{-
import Dependent.Size
import Numeric.Layer
import Numeric.Layer.Convolution
import Numeric.Vector.Sized

newtype Unbatched stride inputSize outputSize kernelSize
  = Unbatched (Strided stride 1 inputSize outputSize kernelSize)

instance Layer (Strided stride 1 (ZZ ::. width ::. height ::. channels) (ZZ ::. resultWidth ::. resultHeight ::. filters) (ZZ ::. kernelWidth ::. kernelHeight))
  => Layer (Unbatched stride (ZZ ::. width ::. height ::. channels) (ZZ ::. resultWidth ::. resultHeight ::. filters) (ZZ ::. kernelWidth ::. kernelHeight)) where
    type Inputs (Unbatched stride (ZZ ::. width ::. height ::. channels) (ZZ ::. resultWidth ::. resultHeight ::. filters) (ZZ ::. kernelWidth ::. kernelHeight)) = ZZ ::. channels ::. height ::. width
    type Outputs (Unbatched stride (ZZ ::. width ::. height ::. channels) (ZZ ::. resultWidth ::. resultHeight ::. filters) (ZZ ::. kernelWidth ::. kernelHeight)) = ZZ ::. filters ::. resultHeight ::. resultWidth
    type Tape (Unbatched stride (ZZ ::. width ::. height ::. channels) (ZZ ::. resultWidth ::. resultHeight ::. filters) (ZZ ::. kernelWidth ::. kernelHeight)) = SizedArray (ZZ ::. channels ::. height ::. width)
    type Gradient (Unbatched stride (ZZ ::. width ::. height ::. channels) (ZZ ::. resultWidth ::. resultHeight ::. filters) (ZZ ::. kernelWidth ::. kernelHeight)) = Unbatched stride (ZZ ::. width ::. height ::. channels) (ZZ ::. resultWidth ::. resultHeight ::. filters) (ZZ ::. kernelWidth ::. kernelHeight)
    forward = undefined
    backward = undefined
    applyGradient = undefined
-}
