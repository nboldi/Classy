{-# LANGUAGE NoImplicitPrelude, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Classy.Prelude 
( module Classy.Prelude
, module Classy.Base
, module Classy.Data.Classes
, module Classy.Data.Boolean
, module Classy.Data.Traversable
, module Classy.Data.List
, module Classy.Data.Map
, module Classy.Data.Maybe
, module Classy.Data.Maybe.Instances
, module Classy.Control.Function
, module Classy.Control.Monad
, module Classy.Control.Monad.Zero
, module Classy.Control.Monad.Or
) where

import Classy.Base
import Classy.Data.Classes
import Classy.Data.Maybe
import Classy.Data.Boolean
import Classy.Data.Traversable
import Classy.Data.List
import Classy.Data.Map
import Classy.Data.Maybe
import Classy.Data.Maybe.Instances
import Classy.Control.Function
import Classy.Control.Monad
import Classy.Control.Monad.Zero
import Classy.Control.Monad.Or

import Data.Maybe (Maybe(..), fromJust)
    

  
 

-- | MonadPlus should satisfy monoid laws with zero as empty
-- and <+> as ++
class (MonadZero mp) => MonadPlus mp where
  (<+>) :: mp a -> mp a -> mp a
 
-- | Represents a computation that can fail
class (Monad m) => MonadFail m where
  fail :: (Show s) => s -> m a

  