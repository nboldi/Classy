{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances, TypeFamilies #-}
module Classy.Data.Boolean 
( Boolean(..), IfBoolean(..), Bool(..)
) where

import Prelude as Base
import Classy.Base

-- | Boolean laws:
-- An instance of the Boolean class must follow the rules of
-- monotone boolean algebra (<https://en.wikipedia.org/wiki/Boolean_algebra>)
class Boolean b where
  true :: b
  false :: b
  not :: b -> b  
  (&&) :: b -> b -> b  
  (||) :: b -> b -> b
  
class Boolean b => IfBoolean b where
  if' :: b -> a -> a -> a
  if' b x y = if toBool b then x else y
  toBool :: b -> Base.Bool
  toBool b = if' b Base.True Base.False

instance Boolean Base.Bool where
  true = True
  false = False
  not True = False
  not False = True
  False && _ = False
  True && b = b
  True || _ = True
  False || b = b
  
instance IfBoolean Base.Bool where
  toBool = id
  
-- | Otherwise 
otherwise :: Boolean b => b
otherwise = true

instance Boolean b => GenericClass b where
  type DefaultImpl b = Bool
  generalize True = true
  generalize False = false
  