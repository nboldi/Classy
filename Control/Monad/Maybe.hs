{-# LANGUAGE NoImplicitPrelude #-}
module Classy.Control.Monad.Maybe () where

import Classy.Data.Maybe
import Classy.Data.Classes
import Classy.Control.Monad

instance Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just (f x)
  
instance Applicative Maybe where
  return = Just
  Just f <*> Just a = Just (f a)
  _ <*> _ = Nothing

instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just x >>= fm = fm x
  
instance MonadZero Maybe where
  zero = Nothing

-- <+> is right-biased
instance MonadPlus Maybe where
  _ <+> Just a = Just a
  m <+> Nothing = m

-- `orElse` is left-biased
instance MonadOr Maybe where
  Nothing `orElse` m = m
  m `orElse` _ = m

