{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Numeric.Layer.Convolution where

import Prelude(pure,Show)
import Numeric.Randomized
import Dependent.Size
import Dependent.Div
import Numeric.Vector.Sized(SizedArray',SizedArray(..),runSized)
import qualified Numeric.Vector.Sized as Sized
import Data.Array.Accelerate
import Data.Array.Accelerate.LLVM.Native(run)
import Data.Array.Accelerate.Numeric.LinearAlgebra((<>), Numeric)
import Data.Proxy
import GHC.TypeLits
import Numeric.Layer

data Convolution (stride :: Nat) (samples :: Nat) (width :: Nat) (height :: Nat) (channels :: Nat) (resultWidth :: Nat) (resultHeight :: Nat) (filters :: Nat) (kernelWidth :: Nat) (kernelHeight :: Nat)
  = Convolution { convolutionKernel :: SizedArray' (ZZ ::. filters ::. channels ::. kernelHeight ::. kernelWidth)
    , convolutionBias :: SizedArray' (ZZ ::. filters ::. resultHeight ::. resultWidth) }
    deriving (Show)

type family Strided stride (samples :: Nat) inputSize outputSize kernelSize = result | result -> stride samples inputSize outputSize kernelSize where
  Strided stride samples (ZZ ::. width ::. height ::. channels) (ZZ ::. resultWidth ::. resultHeight ::. filters) (ZZ ::. kernelWidth ::. kernelHeight)
    = Convolution stride samples width height channels resultWidth resultHeight filters kernelWidth kernelHeight

type CanConvolve stride n w h c rw rh f kw kh
  = (KnownNat stride
  , KnownNat n
  , KnownNat w
  , KnownNat h
  , KnownNat c
  , KnownNat rw
  , KnownNat rh
  , KnownNat f
  , KnownNat kw
  , KnownNat kh
  , rw ~ (((w - kw + 2 * Half kw) `Div` stride) + 1)
  , rh ~ (((h - kh + 2 * Half kh) `Div` stride) + 1)
    )
instance (CanConvolve stride n w h c rw rh f kw kh) =>  Layer (Convolution stride n w h c rw rh f kw kh) where
  type Inputs (Convolution stride n w h c rw rh f kw kh) = ZZ ::. n ::. c ::. h ::. w
  type Outputs (Convolution stride n w h c rw rh f kw kh) = ZZ ::. n ::. f ::. rh ::. rw
  type Tape (Convolution stride n w h c rw rh f kw kh) = SizedArray (ZZ ::. n ::. c ::. h ::. w)
  type Gradient (Convolution stride n w h c rw rh f kw kh) = Convolution stride n w h c rw rh f kw kh
  forward (Convolution kernel bias) x@(Sized xs) = (y, x)
    where
      kernel' = getArray . Sized.use $ kernel
      bias' = getArray . Sized.use $ bias
      y = Sized $ batchCorrelate stride bias' kernel' xs
      stride = constant . fromInteger . natVal $ (Proxy :: Proxy stride)
  backward (Convolution kernel bias) tape@(Sized xs) dy = undefined
  applyGradient (Convolution kernel bias) grad@(Convolution dk db)
    = Convolution
      (runSized run $ Sized.zipWith (-) (Sized.use kernel) (Sized.use dk))
      (runSized run $ Sized.zipWith (-) (Sized.use bias) (Sized.use db))

instance (CanConvolve stride n w h c rw rh f kw kh) =>  Randomized (Convolution stride n w h c rw rh f kw kh) where
  randomized = do
    kernel <- randomized
    bias <- randomized
    pure (Convolution kernel bias)
w2row :: Exp DIM4 -> Exp DIM2 -> Exp DIM4
w2row (unlift -> Z :. _filters :. channels :. kh :. kw :: EDIM4) (unlift -> Z :. row :. col :: EDIM2)
  = lift $ Z :. f :. c :. dy :. dx
  where
    f            = row
    Z:.c:.dy:.dx = unlift (fromIndex (lift $ Z :. channels :. kh :. kw) col) :: EDIM3
type EDIM0 = Z
type EDIM1 = EDIM0 :. Exp Int
type EDIM2 = EDIM1 :. Exp Int
type EDIM3 = EDIM2 :. Exp Int
type EDIM4 = EDIM3 :. Exp Int
type EDIM5 = EDIM4 :. Exp Int
type EDIM6 = EDIM5 :. Exp Int

batchCorrelate
  :: forall e
   . (Num e, Numeric e)
  => Exp Int
  -> Acc (Array DIM3 e)
  -> Acc (Array DIM4 e)
  -> Acc (Array DIM4 e)
  -> Acc (Array DIM4 e)
batchCorrelate stride bias kernel xs = result
  where
    kernelSize                   = shape kernel
    Z:.samples:.channels:.h :.w  = unlift (shape xs) :: EDIM4
    Z:.filters:._       :.kh:.kw = unlift kernelSize :: EDIM4
    -- calculate amount of padding
    (hpad, wpad)                 = (kh `div` 2, kw `div` 2) :: (Exp Int, Exp Int)
    pad (unlift -> Z :. n :. c :. y :. x :: EDIM4) =
      lift (Z :. n :. c :. y + hpad :. x + wpad) :: Exp DIM4
    -- pad image with zeroes in the margin
    zeroes = fill (lift $ Z :. samples :. channels :. h + (2 * hpad) :. w + (2 * wpad)) 0
    padded         = permute const zeroes pad xs
    -- calculate resulting size
    resultHeight   = (h - kh + 2 * hpad) `div` stride + 1
    resultWidth    = (w - kw + 2 * wpad) `div` stride + 1
    resultSize     = lift $ Z :. samples :. filters :. resultHeight :. resultWidth
    -- calculate size of image formatted into columns
    colHeightShape = lift $ Z :. channels :. kh :. kw
    colWidthShape  = lift $ Z :. samples :. resultHeight :. resultWidth
    colSize        = lift $ Z :. (shapeSize colHeightShape) :. (shapeSize colWidthShape)
    -- use im2col on to create columns of each convoluted region (for all channels)
    batchCol (unlift -> Z :. row :. col :: EDIM2) =
      lift $ Z :. n :. c :. (y * stride) + dy :. (x * stride) + dx
      where
        Z:.c:.dy:.dx = unlift $ fromIndex colHeightShape row :: EDIM3
        Z:.n:.y :.x  = unlift $ fromIndex colWidthShape col :: EDIM3
    xCol       = backpermute colSize batchCol padded
    -- calculate size of weights corresponding to image values
    rowHeight  = filters
    rowWidth   = (shapeSize colHeightShape)
    rowSize    = lift $ Z :. rowHeight :. rowWidth
    -- restructure the kernel weights such that the correct weight is multiplied with the
    -- correct value in the xCol
    wrow       = w2row kernelSize
    wRow       = backpermute rowSize wrow kernel
    -- multiply the two matrices
    multiplied = wRow <> xCol
    -- reshape result to expected output size
    structure :: Exp DIM4 -> Exp DIM2
    structure (unlift -> Z :. n :. f :. y :. x :: EDIM4) = lift $ Z :. row :. col
      where
        row           = f
        resultColSize = lift $ Z :. samples :. resultHeight :. resultWidth
        col           = toIndex resultColSize (lift $ Z :. n :. y :. x)
    reshaped = backpermute resultSize structure multiplied
    -- finally, add the output bias to the result
    bias' :: Acc (Array DIM4 e)
    bias'  = replicate (lift $ Z :. samples :. All :. All :. All) bias
    result = zipWith (+) reshaped bias'

convFC
  :: Acc (Array DIM4 Double)
  -> Acc (Array DIM3 Double)
  -> Acc (Array DIM4 Double)
  -> Acc (Array DIM4 Double)
convFC kernel bias xs = result where result = batchCorrelate 1 bias kernel xs

