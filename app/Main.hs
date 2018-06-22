{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Lib
import Numeric.Vector.Sized
import Numeric.Layer.Affine
import Numeric.Network
import Numeric.Layer.Affine
import Numeric.Layer.Sigmoid
import Numeric.Loss.MeanSquaredError
import qualified Data.Array.Accelerate.Interpreter as AI
import qualified Data.Array.Accelerate.LLVM.Native as Native

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

net :: Network '[Affine 3 2, Sigmoid 2, Affine 2 2, Sigmoid 2]
net = Affine ws b :~> Sigmoid () :~> Affine ws' b' :~> Last (Sigmoid ())
  where
    ws  = fromList [1, 2, 3, 4, 5, 6]
    b   = fromList [1, 2]
    ws' = fromList [-2, 2, 0, 1]
    b'  = fromList [0, 1]

main :: IO ()
main = do
  print (runUsing Native.run $ predict net (use $ fromList [-1, -2, -3]))
  -- print (runUsing Native.run v)
  -- print (runUsing Native.run (xs >< ys))
  -- print (runUsing Native.run (xs #> v))
  -- print (runUsing Native.run (u <# zs))
  -- print (runUsing Native.run aff)
