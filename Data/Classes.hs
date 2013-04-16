{-# LANGUAGE NoImplicitPrelude #-}
module Classy.Data.Classes where

import Classy.Base

-- | Something that has a zero element and addition
--
-- Empty is neutral:
--
-- > empty + b = b
-- > b + empty = b
--
-- Associativity:
--
-- > (a + b) + c = a + (b + c)
class Monoid m where
  empty :: m
  (+) :: m -> m -> m
  
-- | Something that has a structure that can be mapped.
-- Functor laws:
-- Identity:
--
-- > map id = id
--
-- Composition:
--
-- > map (f . g) = map f . map g
class Functor f where
  map :: (a -> b) -> f a -> f b

-- | A functor that can hold functions that can be applied in the context.
--
-- Applicative laws:
--
-- Identity:
--
-- > return id <*> v = v
--
-- Composition:
--
-- > (return (.) <*> u <*> v) <*> w = u <*> (v <*> w)
--
-- Homomorphism:
--
-- > return f <*> return x = return (f x)
--
-- Interchange:
--
-- > u <*> return y = return (\f -> f y) <*> u
--
infixl 4 <*>  
infixl 4 <$>  
infixl 4 >> 
infixl 4 << 

class (Functor p) => Applicative p where
  -- | Lift a value into the context.
  return   :: a -> p a               
  -- | Perform a function application in the context.
  (<*>)  :: p (a -> b) -> p a -> p b 
  -- | Lift a function to the context and perform application.
  (<$>)  :: (a -> b) -> p a -> p b   
  pa <$> pb = return pa <*> pb
  -- | Function lifting when the result is independent of the first.
  (>>)   :: p a -> p b -> p b        
  pa >> pb = const id <$> pa <*> pb
  -- | Function lifting when the result is independent of the second. 
  (<<)   :: p b -> p a -> p b       
  pa << pb = flip (const id) <$> pa <*> pb

