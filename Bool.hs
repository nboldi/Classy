{-# LANGUAGE NoImplicitPrelude #-}
module Classy.Bool where

-- | Boolean laws:
--   An instance of the Boolean class must follow the rules of
--   monotone boolean algebra (https://en.wikipedia.org/wiki/Boolean_algebra)
class Boolean b where
  true :: b
  false :: b
  not :: b -> b  
  (&&) :: b -> b -> b  
  (||) :: b -> b -> b

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
  
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y