{-# LANGUAGE NoImplicitPrelude, TypeFamilies #-}
module Classy.Base 
( module Classy.Base
, id, const, flip, String
, error, ($), Bool(..)
) where

import Prelude
import Debug.Trace

-- | The class has a default implementation
class GenericClass c where
  type DefaultImpl c :: *
  generalize :: DefaultImpl c -> c