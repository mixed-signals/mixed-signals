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
import Dependent.Size(Size(..), ShapeSize(..),shapeOf)
import Prelude(Double,Int,(.))
import qualified Data.Array.Accelerate as Acc
import Data.Array.Accelerate(Acc,Array,Vector,Shape(..),(+),zipWith,Exp,(:.)(..),Z(..),All(..))
import Data.Proxy(Proxy(..))
import GHC.TypeLits(KnownNat,natVal)
import Data.Kind(Type)
#ifdef ACCELERATE_BLAS
import qualified Data.Array.Accelerate.Numeric.LinearAlgebra as BLAS
#else
#endif


data Sized (size :: Size)
  = Sized {getArray :: Acc (Array (ShapeOf size) Double)}

type SizedVector n = Sized ('ZZ '::. n)
type SizedMatrix n m = Sized ('ZZ '::. n '::. m)

-- [] -> Sized (ZZ ::. 3 ::. 4)
fromList :: forall size . ShapeSize (size :: Size) => [Double] -> Sized size
fromList xs
  | vol Prelude.== len = (Sized . Acc.use . Acc.fromList shape) xs
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

(.+.) :: ShapeSize size => Sized size -> Sized size -> Sized size
Sized v .+. Sized w = Sized (zipWith (+) v w)
-- lift1 :: (Exp sh :. Exp Int -> Exp sh :. Exp Int) -> (Exp (sh :. Int) -> Exp (sh :. Int))
(><)
  :: forall n m k
   . (KnownNat n, KnownNat m, KnownNat k)
  => SizedMatrix n m
  -> SizedMatrix m k
  -> SizedMatrix n k
#ifdef ACCELERATE_BLAS
Sized xs >< Sized ys = Sized (xs BLAS.<> ys)
#else
Sized xs >< Sized ys = Sized zs
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

(<#) :: forall n m. (KnownNat n, KnownNat m) => SizedVector n -> SizedMatrix n m -> SizedVector m
Sized x <# ys = result
  where
     result = Sized (Acc.reshape shape zs')
     shape = Acc.lift (shapeOf result)
     Sized zs' = xs >< ys
     xs' = Acc.reshape (Acc.lift (shapeOf xs)) x
     xs = Sized xs' :: SizedMatrix 1 n

(#>) :: forall n m. (KnownNat n, KnownNat m) => SizedMatrix n m -> SizedVector m -> SizedVector n
xs #> Sized y = result
  where
      result = Sized (Acc.reshape shape zs')
      shape = Acc.lift (shapeOf result)
      Sized zs' = xs >< ys
      ys' = Acc.reshape (Acc.lift (shapeOf ys)) y
      ys = Sized ys' :: SizedMatrix m 1

