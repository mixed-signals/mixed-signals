{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Numeric.Layer.Affine where

import Dependent.Size
import Numeric.Layer
import Numeric.Vector.Sized
import GHC.TypeLits

data Affine (n :: Nat) (m :: Nat) = Affine (SizedMatrix n m) (SizedVector m)

instance (KnownNat n, KnownNat m) => Layer (Affine n m) where
  type Inputs (Affine n m) = ZZ '::. n
  type Outputs (Affine n m) = ZZ '::. m
  forward (Affine ws b) x = affine ws b x

affine
  :: (KnownNat n, KnownNat m)
  => SizedMatrix n m
  -> SizedVector m
  -> SizedVector n
  -> SizedVector m
affine ws b x = (x <# ws) .+. b

-- affine_dw dedy = dedw
-- affine_db dedy = dedb
-- affine_dx dedy = dedx
