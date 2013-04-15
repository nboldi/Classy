{-# LANGUAGE NoImplicitPrelude #-}
-- | The list monad is a calculation that produces the cross
-- product for all the possible combinations.
module Classy.Control.Monad.List () where

import Classy.Base
import Classy.Data.List
import Classy.Data.Classes
import Classy.Data.Traversable
import Classy.Control.Monad

instance Monad [] where
  xs >>= f = concat (map f xs)
  
instance MonadZero [] where
  zero = []
  
instance MonadOr [] where
  [] `orElse` m = m
  a `orElse` _ = a
  
instance MonadPlus [] where
  a <+> b = a + b
