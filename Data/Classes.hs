{-# LANGUAGE NoImplicitPrelude #-}
module Classy.Data.Classes where

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