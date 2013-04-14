{-# LANGUAGE NoImplicitPrelude, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
module Classy.Data.List where

import Classy.Base
import Classy.Data.Traversable
import Classy.Data.Boolean
import Classy.Control.Function
import Classy.Control.Monad

instance Traversable [a] where
  type ElemType [a] = a
  first [] = error "Calling `first` on empty list."
  first (x:_) = x
  rest [] = error "Calling `rest` on empty list."
  rest (_:xs) = xs
  null [] = true
  null _ = false
  
instance Functor [] where
  map f [] = []
  map f (x:xs) = f x : map f xs