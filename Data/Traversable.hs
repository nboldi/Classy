{-# LANGUAGE NoImplicitPrelude, TypeFamilies, FlexibleInstances #-}

module Classy.Data.Traversable where

class Traversable t where
  type ElemType t :: *
  first :: t -> ElemType t
  rest :: t -> t
  