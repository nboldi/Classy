{-# LANGUAGE NoImplicitPrelude, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module Classy.Data.List where

import Classy.Prelude
import Classy.Data.Traversable
import qualified Prelude as Base

instance Traversable [a] where
  type ElemType [a] = a
  first = Base.head
  rest = Base.tail
  
instance Functor [] where
  map f [] = []
  map f (x:xs) = f x : map f xs