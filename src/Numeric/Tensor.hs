{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
module Numeric.Tensor where

import qualified Prelude
import Control.Category
import Data.Kind(Type)
import Dependent.Size
import Data.Array.Accelerate((+),(*))
import qualified Data.Array.Accelerate as Acc
import Numeric.Vector.Sized

class Semigroup g where
  (<>) :: g -> g -> g

-- Associativity:
-- (a <> b) <> c = a <> (b <> c)

class Semigroup m => Monoid m where
  zero :: m

-- Identity:
-- a <> zero = a = zero <> a

class Monoid g => Group g where
    negate :: g -> g

-- Additive inverse
-- a <> negate a = zero = negate a <> a

-- Multiplication
class Group r => Ring r where
  one :: r
  (.*.) :: r -> r -> r

-- one .*. x = x = x .*. one

class Ring k => Field k where
  recip :: k -> k

-- x .*. recip x = one = recip x .*. x

class (Field k, Group v) => Module k v where
  (*.) :: k -> v -> v
  (.*) :: v -> k -> v

newtype Vect k v w = Vect {linear :: v -> w}


instance ShapeSize size => Semigroup (Sized size) where
  -- (<>) :: Sized size -> Sized size -> Sized size
  x <> y = zipWith (+) x y

instance ShapeSize size => Monoid (Sized size) where
  -- zero :: Sized size
  zero = generate (\_shape -> 0)

instance ShapeSize size => Group (Sized size) where
  negate v = map Acc.negate v

instance Semigroup (Acc.Exp Acc.Double) where
  a <> b = a + b

instance Monoid (Acc.Exp Acc.Double) where
  zero = 0

instance Group (Acc.Exp Acc.Double) where
  negate = Acc.negate

instance Ring (Acc.Exp Acc.Double) where
  one = 1
  a .*. b = a * b

instance Field (Acc.Exp Acc.Double) where
  recip x = Acc.recip x

instance (ShapeSize size) => Module (Acc.Exp Acc.Double) (Sized size) where
   r *. v = map (* r) v
   v .* r = map (* r) v

instance Field k => Category (Vect k) where
  id = Vect id
  Vect f . Vect g = Vect (f . g)

class Module k v => VectorSpace k v where
  type Basis k v :: Type
  type (v ⊗ w) :: Type

instance ShapeSize size => VectorSpace (Acc.Exp Double) (Sized size) where
  type (Sized size1 ⊗ Sized size2) = Sized (TensorSize size1 size2)
