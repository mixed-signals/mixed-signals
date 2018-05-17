{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Lib
import qualified Data.Array.Accelerate.Interpreter as AI
import qualified Data.Array.Accelerate.LLVM.Native as AN

v :: SizedVector 3
v = fromList [1, 2, 3] .+. fromList [2, 3, 4]

main :: IO ()
main = print (runUsing AN.run v)
