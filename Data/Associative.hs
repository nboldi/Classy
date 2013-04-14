{-# LANGUAGE NoImplicitPrelude, TypeFamilies, FlexibleInstances, UndecidableInstances #-}
module Classy.Data.Associative where
  
import Classy.Control.Function
import Classy.Data.Maybe

class (Function g, AssocKey g ~ FunArg g, AssocValue g ~ FunRes g) => Associative g where
  type AssocKey g :: *
  type AssocValue g :: *
  
  -- | Unsafe index: will raise an exception when the key is not in the key set
  (!) :: g -> AssocKey g -> AssocValue g
  (!) = apply
  
  -- | Safe indexing: will return Nothing when the key is not in the key set
  at :: g -> AssocKey g -> Maybe (AssocValue g)
  at = partial
