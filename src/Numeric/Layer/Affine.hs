{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Layer.Affine where

import Dependent.Size
import Numeric.Layer
import Numeric.Vector.Sized
import qualified Numeric.Vector.Sized as Sized
import GHC.TypeLits
import Data.Array.Accelerate.LLVM.Native
import Numeric.Randomized
import Data.Proxy
import Control.Monad.Random

data Affine (n :: Nat) (m :: Nat) = Affine (SizedMatrix' n m) (SizedVector' m)

instance (KnownNat n, KnownNat m, 1 <= n, 1 <= m) => Show (Affine n m) where
  showsPrec d (Affine ws b) = showString "Affine" . showsPrec d (ws, b)

instance (KnownNat n, KnownNat m, 1 <= n, 1 <= m) => Layer (Affine n m) where
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
  :: (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
  => SizedMatrix n m
  -> SizedVector m
  -> SizedVector n
  -> SizedVector m
affine ws b x = (x <# ws) .+. b

instance forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m) => Randomized (Affine n m) where
  randomized = do
    ws <- randomized
    bs <- randomized
    pure (Affine ws bs)
