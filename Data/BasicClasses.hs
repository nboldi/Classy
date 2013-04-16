{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, TypeOperators, DefaultSignatures #-}
module Classy.Data.BasicClasses 
( Base.Eq, Base.Ord, Base.Show, Base.Read, Base.Enum, Base.Bounded
) where

import qualified Prelude as Base
import Classy.Base
import Classy.Data.Boolean
import GHC.Generics
