{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Dependent.Div where

import GHC.TypeNats

type family If (c :: Bool) (t :: k) (f :: k) where
  If 'True t _f = t
  If 'False _t f = f

type family Div (x :: Nat) (y :: Nat) :: Nat where
  Div n n = 1
  Div n d = If (n <=? 1+ d) 0 (1 + Div (n-d) d)

type Half n = Div n 2
