{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Loss.MeanSquaredError where

import Numeric.Loss
import Dependent.Size
import qualified Numeric.Vector.Sized as Sized
import Numeric.Vector.Sized(SizedVector',SizedVector)
import Data.Proxy
import GHC.TypeLits(KnownNat,Nat,natVal)

newtype MeanSquaredError (n :: Nat) = MeanSquaredError ()

instance forall n. KnownNat n => Loss (ZZ ::. n) (MeanSquaredError n) where
  -- forward (MeanSquaredError targets) x
  --   = (err, x)
  --     where
  --       err = (normalize . Sized.foldAll (+) 0 . Sized.map square) (Sized.zipWith (-) x (Sized.use targets))
  --       square x = x ^2
  --       n = fromInteger $ natVal (Proxy :: Proxy (n :: Nat))
  --       normalize total = Sized.map (\t -> t / n) total

  lossDerivative _ x targets = dx
    where
      dx = normalize (Sized.zipWith (-) x targets)
      n = fromInteger $ natVal (Proxy :: Proxy (n :: Nat))
      normalize zs = Sized.map (\z -> 2/n * z) zs
