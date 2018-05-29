{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Lib
import Numeric.Vector.Sized
import Numeric.Layer.Affine
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

main :: IO ()
main = do
  print (getArray v)
  print (runUsing Native.run v)
  print (runUsing Native.run (xs >< ys))
  print (runUsing Native.run (xs #> v))
  print (runUsing Native.run (u <# zs))
  print (runUsing Native.run aff)
