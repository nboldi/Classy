{-# LANGUAGE NoImplicitPrelude, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Classy.Control.Function 
( module Classy.Control.Function
, id, flip, const
) where

import Classy.Data.Maybe
import Classy.Base

-- | Something that can act as a function.
-- Minimal complete definition: apply or partial
class Function f where
  type FunArg f :: *
  type FunRes f :: *
  apply :: f -> (FunArg f -> FunRes f)
  apply f = fromJust . partial f
  partial :: f -> (FunArg f -> Maybe (FunRes f))
  partial f = Just . apply f

instance Function (a -> b) where
  type FunArg (a -> b) = a
  type FunRes (a -> b) = b
  apply = id
  partial f = Just . f

-- | Something that can be inverted
-- The type of the inverse can be different from the type of the function
class ( Function f, Function (FunInv f)
      , FunArg f ~ FunRes (FunInv f)
      , FunRes f ~ FunArg (FunInv f) ) => Bijection f where
  type FunInv f :: *
  inverse :: f -> FunInv f
  
-- | Transformations that can be concatenated
class ( Function f, Function g
      , FunRes f ~ FunArg g
      , FunArg f ~ FunArg (FunConc f g)
      , FunRes g ~ FunRes (FunConc f g)) 
      => Composable f g where
  type FunConc f g :: *
  (.) :: g -> f -> FunConc f g
  
instance Composable (a -> b) (b -> c) where
  type FunConc (a -> b) (b -> c) = (a -> c)
  (f . g) x = f (g x)
