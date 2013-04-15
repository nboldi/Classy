{-# LANGUAGE NoImplicitPrelude #-}
-- | Contains the Monad class, and MonadZero, MonadOr and MonadPlus monad types.
module Classy.Control.Monad where

import Classy.Base
import Classy.Data.Classes
import Classy.Control.Function

-- | Monad is a structure that represent computations. 
-- A type with a monad structure defines what it means 
-- to chain operations, or nest functions of that type 
-- together.
-- Minimal complete definition: one of join or (>>=).
class (Applicative m) => Monad m where
  -- | Sequentially compose two actions, passing any value produced
  -- by the first as an argument to the second.
  (>>=) :: m a -> (a -> m b) -> m b
  -- | Combines levels of actions into a single chain of actions.
  join  :: m (m a) -> m a
 
  ma >>= k  = join (map k ma)
  join mma  = mma >>= id  
  
-- | MonadZero is a monad that has a zero element that 
-- represents interrupt of the computation.
--
-- Left catch:
--
-- > (zero >>= f) = zero
class (Monad mz) => MonadZero mz where
  zero :: mz a  

-- | MonadOr is a monad where there is an orElse operation, which can combine multiple calculations in the way that the result is the first not interrupted computation.
--
-- Associativity: 
--
-- > ma `orElse` (mb `orElse` mc) = (ma `orElse` mb) `orElse` mc
--
-- Zero is neutral:
--
-- > ma `orElse` mb = ma = zero `orElse` ma
class (MonadZero mo) => MonadOr mo where
  orElse :: mo a -> mo a -> mo a
  
  
-- | MonadOr is a monad where computations can be concatenated to combine them.
--
-- > zero <+> m = m
-- > m <+> zero = m
--
-- Left distributivity with >>=:
--
-- > (a <+> b) >>= f = (a >>= f) <+> (b >>= f)
--
-- Associativity:
--
-- > (a + b) + c = a + (b + c)
class (MonadZero mp) => MonadPlus mp where
  (<+>) :: mp a -> mp a -> mp a
 
-- | Represents a computation that can fail
class (Monad m) => MonadFail m where
  fail :: (Show s) => s -> m a

  