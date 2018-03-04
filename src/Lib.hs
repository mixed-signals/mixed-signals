{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where
import qualified Prelude
import Data.Array.Accelerate-- (Exp(..),Acc, Array(..),DIM1,DIM2,Num,Floating,Scalar,Z,(:.))
import Data.Array.Accelerate.Interpreter(run)
import Debug.Trace

sigmoid :: (Prelude.Floating e) => e -> e
sigmoid x = 1 / (1 + exp(-x))


(<.>) :: forall e. Num e => Acc (Array DIM2 e) -> Acc (Array DIM2 e) -> Acc (Array DIM2 e)
xs <.> ys = result
        where
            -- xs :: n x m
            -- ys :: m x p
            (Z :. n :. m) = unlift (shape xs) :: Z :. Exp Int :. Exp Int
            (Z :. _ :. p) = unlift (shape ys) :: Z :. Exp Int :. Exp Int
            -- xs' :: n x m x p
            -- ys' :: n x m x p
            xs' = replicate (lift (Z :. All :. All :. p)) xs
            ys' = replicate (lift (Z :. n :. All :. All)) ys
            -- zs :: n x m x p
            zs = zipWith (*) xs' ys'
            -- transposed :: n x p x m
            transposed = backpermute (lift (Z :. n :. p :. m)) permutation zs :: Acc (Array DIM3 e)
                where
                    permutation :: Exp DIM3 -> Exp DIM3
                    permutation sh' = sh
                        where
                            Z :. ni :. pi :. mi = unlift sh'
                            sh = lift (Z :. ni :. mi :. pi :: Z :. Exp Int :. Exp Int :. Exp Int)
            -- result :: n x p
            result = fold (+) 0 transposed

classify :: (Num e, Ord e) => Exp e -> Exp e
classify x = cond ((x * 3 - 8) < 0) 0 1


-- Exp DIM2 == Exp (Z :. Int :. Int)
-- Z :. Exp Int :. Exp Int

colVector :: Elt e => Acc (Array DIM1 e) -> Acc (Array DIM2 e)
colVector x = reshape sh x
    where
        (Z :. n) = unlift (shape x) :: Z :. Exp Int
        sh = lift (Z  :. n :. (1 :: Exp Int)) :: Exp DIM2

rowVector :: Elt e => Acc (Array DIM1 e) -> Acc (Array DIM2 e)
rowVector x = reshape sh x
    where
        (Z :. n) = unlift (shape x) :: Z :. Exp Int
        sh = lift (Z :. (1 :: Exp Int) :. n) :: Exp DIM2

square :: Num e => e -> e
square x = x * x

network :: (Num e, Elt e, Floating e) => Acc (Array DIM1 e) -> Acc (Array DIM2 e) -> Acc (Array DIM2 e) -> Exp e
network x w1 w2 t
    = err
    where
        -- w1 :: n x m
        -- x :: n
        x2 = rowVector x <.> w1
        -- x2 :: 1 x m
        x3 = map sigmoid x2
        -- x3 :: 1 x m
        -- w2 :: m x p
        x4 = x3 <.> w2
        -- x4 :: 1 x p
        x5 = map sigmoid x4
        y = reshape (shape t) x5
        err = 1/(2 * fromIntegral (size y)) * the (fold (+) 0 (map square (zipWith (-) y t)))
        -- dy = 

testNetwork :: Acc (Scalar Double)
testNetwork
    = unit err
    where
        x = use (fromList (Z :. 1) [3]) :: Acc (Array DIM1 Double)
        w1 = use (fromList (Z :. 1 :. 2) [0.3, 0.3])
        w2 = use (fromList (Z :. 2 :. 1) [0.3, 0.3])
        t = map classify x -- t :: Z :. 1
        err = network x w1 w2 t


someFunc :: Prelude.IO ()
someFunc = Prelude.print (run testNetwork)
