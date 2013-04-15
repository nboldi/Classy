{-# LANGUAGE NoImplicitPrelude, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
module Classy.Data.List where

import Classy.Base
import Classy.Data.Traversable
import Classy.Data.Boolean
import Classy.Data.Classes
import Classy.Control.Function
import Classy.Control.Monad

instance Traversable [a] where
  type ElemType [a] = a
  head [] = error "Calling `first` on empty list."
  head (x:_) = x
  tail [] = error "Calling `rest` on empty list."
  tail (_:xs) = xs
  null [] = true
  null _ = false
 
instance Functor [] where
  map _ [] = []
  map f (x:xs) = f x : map f xs
  
instance Monoid [a] where
  empty = []
  [] + ys = ys
  (x:xs) + ys = x : (xs + ys)

instance Applicative [] where
  return a = [a]
  fs <*> xs = concat $ map (\f -> map f xs) fs