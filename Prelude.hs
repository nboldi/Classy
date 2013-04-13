{-# LANGUAGE NoImplicitPrelude, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Classy.Prelude (
  id, const, flip, (.), String, Ord, Show, Read,
  Maybe(..),
  module Classy.Prelude
) where

import Prelude (id, const, flip, String, Ord, Show, Read) 
import Data.Maybe (Maybe(..), fromJust)
import Classy.Bool
  
-- | Something that can act as a function.
-- Minimal complete definition: apply or partial
class Function f where
  type FunArg f :: *
  type FunRes f :: *
  apply :: f -> (FunArg f -> FunRes f)
  apply f = fromJust . partial f
  partial :: f -> (FunArg f -> Maybe (FunRes f))
  partial f = Just . apply f

instance Function (a -> b) where
  type FunArg (a -> b) = a
  type FunRes (a -> b) = b
  apply = id

-- | Something that can be inverted
-- The type of the inverse can be different from the type of the function
class ( Function f, Function (FunInv f)
      , FunArg f ~ FunRes (FunInv f)
      , FunRes f ~ FunArg (FunInv f) ) => Bijection f where
  type FunInv f :: *
  inverse :: f -> FunInv f
  
-- | Transformations that can be concatenated
class ( Function f, Function g
      , FunRes f ~ FunArg g
      , FunArg f ~ FunArg (FunConc f g)
      , FunRes g ~ FunRes (FunConc f g)) 
      => Composable f g where
  type FunConc f g :: *
  (.) :: g -> f -> FunConc f g
  
instance Composable (a -> b) (b -> c) where
  type FunConc (a -> b) (b -> c) = (a -> c)
  (f . g) x = f (g x)
  
-- | Something that has a zero element and addition
-- Monoid laws:
--   empty ++ b = b
--   b ++ empty = b                            (zero laws)
--   (a ++ b) ++ c = a ++ (b ++ c)             (associativity)
--   (a ++ b) >>= f = (a >>= f) ++ (b >>= f)   (left distribution)
class Monoid m where
  empty :: m
  (++) :: m -> m -> m

  
class Num m where
  
  
-- | Something that has a structure that can be mapped.
-- Functor laws:
--   map id = id
--   map (f.g) = map f . map g
class Functor f where
  map :: (a -> b) -> f a -> f b
  
infixl 4 <@>  

class (Functor p) => Applicative p where
  return   :: a -> p a               -- value lifting
  (<@>)  :: p (a -> b) -> p a -> p b -- lifted application
  
  (<$>)  :: (a -> b) -> p a -> p b   -- function lifting
  pa <$> pb = return pa <@> pb
  
  (>>)   :: p a -> p b -> p b        -- when the result is independent of the first
  pa >> pb = const id <$> pa <@> pb
  (<<)   :: p b -> p a -> p b        -- when the result is independent of the second  
  pa << pb = flip (const id) <$> pa <@> pb

  
class (Applicative m) => Monad m where
  -- | Minimal complete definition: one of join or (>>=).
  (>>=) :: m a -> (a -> m b) -> m b -- bind
  join  :: m (m a) -> m a           -- combining levels of structure
 
  ma >>= k  = join (map k ma)
  join mma  = mma >>= id  
  
-- | MonadZero laws:
--  zero >>= f = zero
class (Monad mz) => MonadZero mz where
  zero :: mz a  

-- | MonadPlus should satisfy monoid laws with zero as empty
-- and <+> as ++
class (MonadZero mp) => MonadPlus mp where
  (<+>) :: mp a -> mp a -> mp a
  
-- | MonadPlus should satisfy monoid laws with zero as empty
-- and orElse as ++
class (MonadZero mo) => MonadOr mo where
  orElse :: mo a -> mo a -> mo a
 
class (Monad m) => MonadFail m where
  fail :: (Show s) => s -> m a
