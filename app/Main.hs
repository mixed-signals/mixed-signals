{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module Main where

import Lib
import GHC.TypeLits
import Numeric.Vector.Sized
import Numeric.Layer.Affine
import Numeric.Network
import Numeric.Layer.Affine
import Numeric.Layer.Sigmoid
import Numeric.Layer.Convolution
import Numeric.Layer.Convolution.Unbatched
import Numeric.Layer.Reshape
import Numeric.Layer.Identity
import Numeric.Loss.MeanSquaredError
import qualified Data.Array.Accelerate.Interpreter as AI
import qualified Data.Array.Accelerate.LLVM.Native as Native
import Control.Monad.Random(MonadRandom,evalRandIO,getRandoms)
import Numeric.Randomized
import Dependent.Size

v :: SizedVector 3
v = use (fromList [1, 2, 3]) .+. use (fromList [2, 3, 4])

xs :: SizedMatrix 2 3
xs = use (fromList [1 .. 6])
-- 1 2 3
-- 4 5 6

ys :: SizedMatrix 3 4
ys = use (fromList [1 .. 12])
-- 1 2 3 4
-- 5 6 7 8
-- 9 10 11 12
--
u = use (fromList [1, 2]) :: SizedVector 2
zs = use (fromList [1, 2, 3, 4]) :: SizedMatrix 2 2

aff = affine xs v u

type Net a b = Affine 3 a ~> Sigmoid a ~> Affine a b ~> Sigmoid b ~> ()
network :: MonadRandom m => m (Net 2 2)
network = randomized


type Conv = Strided 2 1 (ZZ ::. 1 ::. 1 ::. 1) (ZZ ::. 1 ::. 1 ::. 1) (ZZ ::. 2 ::. 2) ~> ()
type BatchedConvolve n
  = Identity (ZZ ::. 4 ::. 4 ::. n)
  ~~> Strided 2 n (ZZ ::. 4 ::. 4 ::. 1) (ZZ ::. 3 ::. 3 ::. 1) (ZZ ::. 2 ::. 2)
  ~> ()
convolve :: (MonadRandom m, KnownNat n, 1 <= n) => m (BatchedConvolve n)
convolve = randomized

main :: IO ()
main = do
  net <- evalRandIO network
  c   <- evalRandIO convolve :: IO (BatchedConvolve 3)
  xs  <- evalRandIO (take (3 * 16) <$> getRandoms)
  evalRandIO (randomized :: MonadRandom m => m Conv) >>= print
  print c
  print (run $ predict c (use $ fromList xs))
  print (training net)
  where
    training net = train
      net
      (MeanSquaredError () :: MeanSquaredError 2)
      (use $ fromList [-1, -2, -3])
      (use $ fromList [1, 2])

  -- print (runUsing Native.run v)
  -- print (runUsing Native.run (xs >< ys))
  -- print (runUsing Native.run (xs #> v))
  -- print (runUsing Native.run (u <# zs))
  -- print (runUsing Native.run aff)
