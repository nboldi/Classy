{-# LANGUAGE NoImplicitPrelude, TypeFamilies, FlexibleInstances #-}

module Classy.Data.Traversable where

import Classy.Data.Boolean

class Traversable t where
  type ElemType t :: *
  first :: t -> ElemType t
  rest :: t -> t
  null :: (Boolean b) => t -> b
  
