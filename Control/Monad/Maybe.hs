{-# LANGUAGE NoImplicitPrelude #-}
-- | The maybe monad represents a calculation that may or may not
-- result in a value.
module Classy.Control.Monad.Maybe () where

import Classy.Data.Maybe
import Classy.Control.Monad

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

