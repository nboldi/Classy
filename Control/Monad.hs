{-# LANGUAGE NoImplicitPrelude #-}
module Classy.Control.Monad where

import Classy.Control.Function

-- | Something that has a structure that can be mapped.
-- Functor laws:
--   map id = id
--   map (f.g) = map f . map g
class Functor f where
  map :: (a -> b) -> f a -> f b
  
infixl 4 <*>  

class (Functor p) => Applicative p where
  return   :: a -> p a               -- value lifting
  (<*>)  :: p (a -> b) -> p a -> p b -- lifted application
  
  (<$>)  :: (a -> b) -> p a -> p b   -- function lifting
  pa <$> pb = return pa <*> pb
  
  (>>)   :: p a -> p b -> p b        -- when the result is independent of the first
  pa >> pb = const id <$> pa <*> pb
  (<<)   :: p b -> p a -> p b        -- when the result is independent of the second  
  pa << pb = flip (const id) <$> pa <*> pb

  
class (Applicative m) => Monad m where
  -- | Minimal complete definition: one of join or (>>=).
  (>>=) :: m a -> (a -> m b) -> m b -- bind
  join  :: m (m a) -> m a           -- combining levels of structure
 
  ma >>= k  = join (map k ma)
  join mma  = mma >>= id  