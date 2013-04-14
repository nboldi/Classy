{-# LANGUAGE NoImplicitPrelude #-}
module Classy.Data.Maybe.Instances () where

import Classy.Data.Maybe
import Classy.Control.Monad
import Classy.Control.Monad.Zero
import Classy.Control.Monad.Or

instance Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just (f x)
  
instance Applicative Maybe where
  return = Just

instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just x >>= fm = fm x
  
instance MonadZero Maybe where
  zero = Nothing
  
instance MonadOr Maybe where
  Nothing `orElse` m = m
  m `orElse` _ = m
