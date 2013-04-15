{-# LANGUAGE NoImplicitPrelude #-}
module Classy.Control.Monad where

import Classy.Base
import Classy.Data.Classes
import Classy.Control.Function

-- | Monad is a structure that represent computations. A type with a monad structure defines what it means to chain operations, or nest functions of that type together. This allows the programmer to build pipelines that process data in steps, in which each action is decorated with additional processing rules provided by the monad.
-- Minimal complete definition: one of join or (>>=).
class (Applicative m) => Monad m where
  -- | sequence of actions
  (>>=) :: m a -> (a -> m b) -> m b
  -- | combining levels of structure
  join  :: m (m a) -> m a
 
  ma >>= k  = join (map k ma)
  join mma  = mma >>= id  
  
-- | MonadZero is a monad that has a zero element that represents interrupt of the computation.
-- MonadZero law:
-- * "zero" is a left zero element:
-- > zero >>= f = zero
class (Monad mz) => MonadZero mz where
  zero :: mz a  

-- | MonadOr is a monad where there is an orElse operation, which can combine multiple calculations in the way that the result is the first not interrupted computation.
-- MonadOr laws:
-- * Associativity: 
-- > ma `orElse` (mb `orElse` mc) = (ma `orElse` mb) `orElse` mc
-- * Zero is neutral:
-- > ma `orElse` mb = ma = zero `orElse` ma
class (MonadZero mo) => MonadOr mo where
  orElse :: mo a -> mo a -> mo a
  
  
-- | MonadOr is a monad where computations can be concatenated to combine them.
-- MonadOr laws:
-- * Zero is neutral:
-- > zero <+> m = m
-- > m <+> zero = m
-- * Left distributivity with >>=:
-- > (a <+> b) >>= f = (a >>= f) <+> (b >>= f)
-- * Associativity:
--   (a + b) + c = a + (b + c)
class (MonadZero mp) => MonadPlus mp where
  (<+>) :: mp a -> mp a -> mp a
 
-- | Represents a computation that can fail
class (Monad m) => MonadFail m where
  fail :: (Show s) => s -> m a

  