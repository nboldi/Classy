{-# LANGUAGE NoImplicitPrelude #-}
module Classy.Control.Monad.Zero where

import Classy.Control.Monad

-- | MonadZero laws:
-- > zero >>= f = zero
class (Monad mz) => MonadZero mz where
  zero :: mz a  