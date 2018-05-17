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

import qualified Prelude
import Data.Kind(Type)
import Prelude(Double,Int,(.))
import Data.Array.Accelerate(Acc,Array,Vector,Shape(..),(+),zipWith)
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as AI
import GHC.TypeLits(Nat,KnownNat,natVal)
import qualified Data.Array.Accelerate.Numeric.LinearAlgebra as LA
import Data.Proxy(Proxy(..))

data Size = ZZ | Size ::. Nat

class (Shape (ShapeOf size)) => ShapeSize size where
  type ShapeOf (size :: Size) :: Type
  shapeOf :: proxy size -> ShapeOf size

instance ShapeSize 'ZZ where
  type ShapeOf 'ZZ = A.Z
  shapeOf _ = A.Z

instance forall size n. (KnownNat n, ShapeSize size) => ShapeSize (size ::. n) where
  type ShapeOf (size ::. n) = (ShapeOf size) A.:. Int
  shapeOf _ = outerShape A.:. n
        where
            outerShape = shapeOf (Proxy :: Proxy size)
            n = Prelude.fromInteger (natVal (Proxy :: Proxy n)) :: Int

data Sized (size :: Size) = Sized { getArray :: Acc (Array (ShapeOf size) Double) }

instance (Prelude.Show (Array (ShapeOf size) Double), Shape (ShapeOf size)) => Prelude.Show (Sized size) where
  show (Sized arr) = Prelude.show arr

type SizedVector n = (KnownNat n) => Sized ('ZZ '::. n)
type SizedMatrix n m = (KnownNat n, KnownNat m) => Sized ('ZZ '::. n '::. m)

fromList :: forall size . ShapeSize (size :: Size) => [Double] -> Sized size
fromList = Sized . A.use . A.fromList shape
  where shape = shapeOf (Proxy :: Proxy (size :: Size))

(.+.) :: SizedVector n -> SizedVector n -> SizedVector n
Sized v .+. Sized w = Sized (zipWith (+) v w)

run :: ShapeSize size => Sized size -> Array (ShapeOf size) Double
run = runUsing AI.run

runUsing
  :: (ShapeSize size, A.Arrays (Array (ShapeOf size) Double))
  => (Acc (Array (ShapeOf size) Double) -> Array (ShapeOf size) Double)
  -> Sized size
  -> Array (ShapeOf size) Double
runUsing runner (Sized arr) = runner arr

-- (<#) :: SizedVector n -> SizedMatrix n m -> SizedVector m
-- Sized v <# Sized m = _

