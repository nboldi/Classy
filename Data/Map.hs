{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}

module Classy.Data.Map where

import Classy.Prelude
import Classy.Data.Associative
import Classy.Data.Traversable
import Classy.Data.Tuple
import Classy.Data.List
import qualified Data.Map as P

class (Traversable g, Associative g) => Map g where
  

instance Ord a => Function (P.Map a b) where
  type FunArg (P.Map a b) = a
  type FunRes (P.Map a b) = b
  apply = (P.!)
  partial = flip P.lookup
  
instance (Ord a, Ord b) => Bijection (P.Map a b) where
  type FunInv (P.Map a b) = P.Map b a
  inverse = P.fromList . map swap . P.toList