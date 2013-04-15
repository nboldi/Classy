{-# LANGUAGE NoImplicitPrelude #-}
module Classy.Data.Maybe 
( Maybe(..), fromMaybe, fromJust, isJust
) where

import Classy.Data.Classes
import Data.Maybe

instance Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just (f x)
  
instance Applicative Maybe where
  return = Just
  Just f <*> Just a = Just (f a)
  _ <*> _ = Nothing
