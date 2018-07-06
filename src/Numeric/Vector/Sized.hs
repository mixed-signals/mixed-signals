{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Vector.Sized where
import qualified Prelude
import Numeric.Randomized
import Control.Monad.Random(getRandoms)
import Dependent.Size(Size(..), ShapeSize(..),shapeOf)
import Prelude(Double,Int,(.))
import qualified Data.Array.Accelerate as Acc
import Data.Array.Accelerate(Acc,Array,Vector,Shape(..),(+),Exp,(:.)(..),Z(..),All(..))
import Data.Proxy(Proxy(..))
import GHC.TypeLits(KnownNat,natVal, type (<=))
import Data.Kind(Type)
#ifdef ACCELERATE_BLAS
import qualified Data.Array.Accelerate.Numeric.LinearAlgebra as BLAS
#else
#endif

-- | Comuptation of a Sized multi-dimensional array
data SizedArray (size :: Size)
  = Sized {getArray :: Acc (Array (ShapeOf size) Double)}

type SizedVector n = SizedArray ('ZZ '::. n)
type SizedMatrix n m = SizedArray ('ZZ '::. n '::. m)

-- | Sized multi-dimensional array (pre-computed)
data SizedArray' (size :: Size)
  = Sized' {getArray' :: Array (ShapeOf size) Double}

instance forall size . ShapeSize size => Randomized (SizedArray' size) where
  randomized = do
    values <- Prelude.fmap (Prelude.take vol) getRandoms
    Prelude.pure (fromList values)
      where
        vol = volume (Proxy :: Proxy size)

instance (ShapeSize size) => Prelude.Show (SizedArray' size) where
  showsPrec n (Sized' arr) = Prelude.showsPrec n arr

type SizedVector' n = SizedArray' ('ZZ '::. n)
type SizedMatrix' n m = SizedArray' ('ZZ '::. n '::. m)

runSized ::
  (Acc (Array (ShapeOf size1) Double)
   -> Array (ShapeOf size2) Double)
  -> SizedArray size1 -> SizedArray' size2
runSized r (Sized x) = Sized' (r x)

generate :: forall size.
  (ShapeSize size)
    => (Exp (ShapeOf size) -> Exp Double)
    -> SizedArray size
generate f = result
  where
    result = Sized (Acc.generate shape f) :: SizedArray size
    shape = Acc.lift (shapeOf result) :: Exp (ShapeOf size)

reshape :: forall size size'.
  (ShapeSize size, ShapeSize size', Volume size ~ Volume size') =>
    SizedArray size -> SizedArray size'
reshape (Sized x) = result
  where
    result = Sized (Acc.reshape shape x)
    shape = Acc.lift (shapeOf result)

map :: ShapeSize size => (Exp Double -> Exp Double) -> SizedArray size -> SizedArray size
map f (Sized x) = Sized (Acc.map f x)

foldAll f z (Sized x) = Sized (Acc.foldAll f z x)

use :: ShapeSize size => SizedArray' size -> SizedArray size
use (Sized' array) = Sized (Acc.use array)

zipWith :: ShapeSize size => (Exp Double -> Exp Double -> Exp Double) -> SizedArray size -> SizedArray size -> SizedArray size
zipWith f (Sized x) (Sized y) = Sized (Acc.zipWith f x y)

outer :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m) => SizedVector n -> SizedVector m -> SizedMatrix n m
outer x y = x' <> y'
  where
    x' = reshape x :: SizedArray (ZZ ::. n ::. 1)
    y' = reshape y :: SizedArray (ZZ ::. 1 ::. m)

-- [] -> Sized (ZZ ::. 3 ::. 4)
fromList :: forall size . ShapeSize (size :: Size) => [Double] -> SizedArray' size
fromList xs
  | vol Prelude.== len = (Sized' . Acc.fromList shape) xs
  | Prelude.otherwise = Prelude.error
    (          "Length of list ("
    Prelude.++ Prelude.show len
    Prelude.++ ") doesn't match type of Sized array:"
    Prelude.++ " ("
    Prelude.++ Prelude.show shape
    Prelude.++ ")"
    )
 where
  len   = Prelude.length xs
  proxy = Proxy :: Proxy (size :: Size)
  shape = shapeOf proxy
  vol   = volume proxy

(.+.) :: ShapeSize size => SizedArray size -> SizedArray size -> SizedArray size
Sized v .+. Sized w = Sized (Acc.zipWith (+) v w)
-- lift1 :: (Exp sh :. Exp Int -> Exp sh :. Exp Int) -> (Exp (sh :. Int) -> Exp (sh :. Int))
(<>)
  :: forall n m k
  . (KnownNat n, KnownNat m, KnownNat k, 1 <= n, 1 <= m, 1 <= k)
  => SizedMatrix n m
  -> SizedMatrix m k
  -> SizedMatrix n k
#ifdef ACCELERATE_BLAS
Sized xs <> Sized ys = Sized (xs BLAS.<> ys)
#else
Sized xs <> Sized ys = Sized zs
 where
  n           = Prelude.fromInteger (natVal (Proxy :: Proxy n)) :: Int
  m           = Prelude.fromInteger (natVal (Proxy :: Proxy m)) :: Int
  k           = Prelude.fromInteger (natVal (Proxy :: Proxy k)) :: Int
  -- Z :. n :. k
  zs          = Acc.fold (Acc.+) (Acc.constant 0) zs'
  -- Z :. n :. k :. m
  zs'         = Acc.zipWith (Acc.*) xs' ys'
  -- Z :. n :. k :. m
  xs'         = Acc.replicate (Acc.lift (Z :. All :. k :. All)) xs
  ys'         = (transpose . Acc.replicate (Acc.lift (Z :. n :. All :. All))) ys
  outputShape = shapeOf (Proxy :: Proxy (ZZ ::. n ::. k ::. m))
  transpose   = Acc.backpermute (Acc.lift outputShape) swap
  swap :: Exp (Z :. Int :. Int :. Int) -> Exp (Z :. Int :. Int :. Int)
  swap        = Acc.lift1 sw
  sw :: Exp Z :. Exp Int :. Exp Int :. Exp Int -> Exp Z :. Exp Int :. Exp Int :. Exp Int
  sw (sh:.y:.x) = sh :. x :. y
#endif

(<#) :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m) => SizedVector n -> SizedMatrix n m -> SizedVector m
Sized x <# ys = result
  where
     result = Sized (Acc.reshape shape zs')
     shape = Acc.lift (shapeOf result)
     Sized zs' = xs <> ys
     xs' = Acc.reshape (Acc.lift (shapeOf xs)) x
     xs = Sized xs' :: SizedMatrix 1 n

(#>) :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m) => SizedMatrix n m -> SizedVector m -> SizedVector n
xs #> Sized y = result
  where
      result = Sized (Acc.reshape shape zs')
      shape = Acc.lift (shapeOf result)
      Sized zs' = xs <> ys
      ys' = Acc.reshape (Acc.lift (shapeOf ys)) y
      ys = Sized ys' :: SizedMatrix m 1

