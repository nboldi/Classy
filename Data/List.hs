{-# LANGUAGE NoImplicitPrelude, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module Classy.Data.List where

import Classy.Prelude
import Classy.Data.Traversable
import qualified Prelude as Base

instance Traversable [a] where
  type ElemType [a] = a
  first [] = error "Calling `first` on empty list."
  first (x:_) = x
  rest [] = error "Calling `rest` on empty list."
  rest (_:xs) = xs
  null [] = True
  null _ = False
  
instance Functor [] where
  map f [] = []
  map f (x:xs) = f x : map f xs