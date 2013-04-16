{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module Classy.Data.Numeric
( Num
, Integral(..), Integer, Int
, Rational, Real, Fractional, Floating
) where

import Prelude as Base
import Classy.Data.Classes

instance Num n => Monoid n where
  empty = 0
  (+) = (Base.+)
{-
-- | Numeric represents values that can be added ('+') and multiplied ('*'),
-- substracted ('-') and have a 'unit' value. 
-- 
-- Numeric laws:
--
-- The numeric instance should also be a monoid with 'unit' as 'empty'
-- and '*' as '+'. This means it should satisfy neutral rules and associativity.
--
-- Distributivity:
--
-- > a * (b+c) = a*b + a*c
-- > (a+b) * c = a*c + b*c
class Monoid n => Numeric n where
  unit :: n
  (*) :: n -> n -> n
  (-) :: n -> n -> n
  negate :: n -> n
  
  x - y =     x + negate y
  negate x =  empty - x

-}
