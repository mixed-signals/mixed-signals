{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dependent.Size where
import Data.Kind(Type)
import GHC.TypeLits(Nat,KnownNat,natVal)
import qualified Data.Array.Accelerate as Acc
import Data.Array.Accelerate(Shape(..))
import Data.Proxy(Proxy(..))

infixl 6 ::.
data Size = ZZ | Size ::. Nat

class (Shape (ShapeOf size)
  , Acc.Lift Acc.Exp (Acc.Plain (ShapeOf size))
  , Acc.Slice (Acc.Plain (ShapeOf size))
  , ShapeOf size ~ Acc.Plain (ShapeOf size)
      ) => ShapeSize size where
  type ShapeOf (size :: Size) :: Type
  shapeOf :: proxy size -> ShapeOf size
  volume :: proxy size -> Int

--            | Value           | Type          | Kind
-- Accelerate | (Z :. 3)        :: (Z :. Int)   :: Type
--            |  ^                 ↑ ShapeOf
-- Sized      |  ^---- shapeOf <-  ZZ ::. 3    :: Size

instance ShapeSize 'ZZ where
  type ShapeOf 'ZZ = Acc.Z
  shapeOf _ = Acc.Z
  volume _ = 1

instance forall size n. (KnownNat n, ShapeSize size) => ShapeSize (size ::. n) where
  type ShapeOf (size '::. n) = (ShapeOf size) Acc.:. Int
  volume _ = outerVolume * n
    where
      outerVolume = volume (Proxy :: Proxy size)
      n = Prelude.fromInteger (natVal (Proxy :: Proxy n)) :: Int
  shapeOf _ = outerShape Acc.:. n
        where
            outerShape = shapeOf (Proxy :: Proxy size)
            n = Prelude.fromInteger (natVal (Proxy :: Proxy n)) :: Int
