{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Numeric.Layer.Affine where

import Dependent.Size
import Numeric.Layer
import Numeric.Vector.Sized
import qualified Numeric.Vector.Sized as Sized
import GHC.TypeLits
import Data.Array.Accelerate.LLVM.Native

data Affine (n :: Nat) (m :: Nat) = Affine (SizedMatrix' n m) (SizedVector' m)

instance (KnownNat n, KnownNat m) => Layer (Affine n m) where
  type Inputs (Affine n m) = ZZ '::. n
  type Outputs (Affine n m) = ZZ '::. m
  type Tape (Affine n m) = SizedArray (ZZ ::. n)
  type Gradient (Affine n m) = Affine n m
  forward (Affine ws b) x = (affine (use ws) (use b) x, x)
  backward (Affine ws b) x dy = (Affine dw db, dx)
    where
      dx = use ws #> dy
      dw = runSized run (dy `outer` x)
      db = runSized run (dy)
  applyGradient (Affine ws b) (Affine dw db)
    = Affine
      (runSized run $ Sized.zipWith (-) (use ws) (use dw))
      (runSized run $ Sized.zipWith (-) (use b) (use db))

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
