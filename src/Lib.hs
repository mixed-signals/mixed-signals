{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( someFunc) where
import qualified Prelude
import Data.Array.Accelerate-- (Exp(..),Acc, Array(..),DIM1,DIM2,Num,Floating,Scalar,Z,(:.))
import Data.Array.Accelerate.Interpreter(run)
import Debug.Trace

sigmoid :: (Prelude.Floating e) => e -> e
sigmoid x = 1 / (1 + exp(-x))

dsigmoid :: (Prelude.Num e) => e -> e
dsigmoid y = y * (1 - y)


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

square :: Prelude.Num e => e -> e
square x = x * x


train :: (Num e, Elt e, Floating e, FromIntegral Int e) => Exp e -> Acc (Array DIM1 e) -> Acc (Array DIM2 e) -> Acc (Array DIM2 e) -> Acc (Array DIM1 e) -> (Exp e, Acc (Array DIM2 e), Acc (Array DIM2 e))
train η x w1 w2 t
    = trace (Prelude.show $ run dy)
    $ trace (Prelude.show $ run dx4)
    $ trace (Prelude.show $ run dw2)
    $ trace (Prelude.show $ run dx3)
    $ trace (Prelude.show $ run dx2)
    $ trace (Prelude.show $ run dw1)
    $ trace (Prelude.show $ run dx1) (err, w1', w2')
    where
        -- w1 :: n x m
        -- x :: n
        x1 = rowVector x
        x2 = x1 <.> w1
        -- x2 :: 1 x m
        x3 = map sigmoid x2
        -- x3 :: 1 x m
        -- w2 :: m x p
        x4 = x3 <.> w2
        -- x4 :: 1 x p
        x5 = map sigmoid x4

        y = reshape (shape t) x5
        z = map (\v -> v / fromIntegral (size y)) $ zipWith (-) y t
        err = 1/2 * the (fold (+) 0 $ map square z)
        dy = rowVector z
        dx4 = zipWith (*) (map dsigmoid x5) dy
        dw2 = transpose x3 <.> dx4
        dx3 = dx4 <.> transpose w2
        dx2 = zipWith (*) (map dsigmoid x3) dx3
        dw1 = transpose x1 <.> dx2
        dx1 = dx2 <.> transpose w1
        update = zipWith (\w dw -> w - η*dw)
        w1' = update w1 dw1
        w2' = update w2 dw2

testNetwork :: Acc (Scalar Double)
testNetwork
    = trace (Prelude.show $ run w1' )
    $ trace (Prelude.show $ run w2' )
    $ unit err
    where
        x = use (fromList (Z :. 1) [3]) :: Acc (Array DIM1 Double)
        w1 = use (fromList (Z :. 1 :. 2) [0.3, 0.3])
        w2 = use (fromList (Z :. 2 :. 1) [0.3, 0.3])
        t = map classify x -- t :: Z :. 1
        lr = 1.0e-2
        (err, w1', w2') = train lr x w1 w2 t


someFunc :: Prelude.IO ()
someFunc = Prelude.print (run testNetwork)
