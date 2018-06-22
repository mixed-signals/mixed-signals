{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib where

import Numeric.Vector.Sized
import Dependent.Size
import qualified Data.Array.Accelerate as Acc
import Data.Array.Accelerate(Acc,Array,Double)
import qualified Data.Array.Accelerate.Interpreter as AI

run :: ShapeSize size => SizedArray size -> Array (ShapeOf size) Double
run = runUsing AI.run

runUsing
  :: (ShapeSize size, Acc.Arrays (Array (ShapeOf size) Double))
  => (Acc (Array (ShapeOf size) Double) -> Array (ShapeOf size) Double)
  -> SizedArray size
  -> Array (ShapeOf size) Double
runUsing runner (Sized arr) = runner arr

