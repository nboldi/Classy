{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances, TypeFamilies #-}
module Classy.Data.Boolean 
( Boolean, Bool(..)
) where

import Classy.Prelude

-- | Boolean laws:
--   An instance of the Boolean class must follow the rules of
--   monotone boolean algebra (https://en.wikipedia.org/wiki/Boolean_algebra)
class Boolean b where
  true :: b
  false :: b
  not :: b -> b  
  (&&) :: b -> b -> b  
  (||) :: b -> b -> b
  
class Selector b where
  if' :: b -> a -> a -> a

data Bool = True | False

instance Boolean Bool where
  true = True
  false = False
  not True = False
  not False = True
  False && _ = False
  True && b = b
  True || _ = True
  False || b = b
  
instance Selector Bool where
  if' True a _ = a
  if' False _ b = b
  
-- | Otherwise 
otherwise :: Boolean b => b
otherwise = true

instance Boolean b => GenericClass b where
  type DefaultImpl b = Bool
  generalize True = true
  generalize False = false
  