{-# LANGUAGE NoImplicitPrelude #-}
module Classy.Control.Monad.Or where

import Classy.Control.Monad
import Classy.Control.Monad.Zero

-- | MonadOr laws:
-- * Associativity: 
-- > ma `orElse` (mb `orElse` mc) = (ma `orElse` mb) `orElse` mc
-- * Zero is neutral:
-- > ma `orElse` mb = ma = zero `orElse` ma
class (MonadZero mo) => MonadOr mo where
  orElse :: mo a -> mo a -> mo a