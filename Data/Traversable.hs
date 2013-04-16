{-# LANGUAGE NoImplicitPrelude, TypeFamilies, FlexibleInstances, FlexibleContexts #-}
module Classy.Data.Traversable where

import Classy.Base
import Classy.Data.Maybe
import Classy.Data.Boolean
import Classy.Data.Numeric
import Classy.Data.Classes
import Classy.Control.Function

-- | A collection that can be deconstructed from element to element.
-- Minimal complete definition: first, rest, null or view
class Traversable t where
  -- | The type of elements in the collection.
  type ElemType t :: *
  -- | Gets an element from the collection.
  head :: t -> ElemType t
  head t = case view t of Just (x,_) -> x
                          Nothing -> error "Called head on empty collection"
  
  -- | Gets the collection without the head element.
  tail :: t -> t
  tail t = case view t of Just (_,xs) -> xs
                          Nothing -> error "Called tail on empty collection"
  
  -- | True, if the collection is empty.
  null :: t -> Bool
  null = isJust . view
  
  -- | Deconstructs the collection into the head and the tail.
  view :: t -> Maybe (ElemType t, t)
  view t = if null t then Nothing
                     else Just (head t, tail t)
  
  last :: t -> ElemType t
  last t = if null t 
             then error "Called last on empty collection." 
             else last' (head t) (tail t)
    where last' e t = if null t then e 
                        else last' (head t) (tail t)
                            

  foldl :: (a -> ElemType t -> a) -> a -> t -> a
  foldl f coll cont = case view cont of
    Just (head,tail) -> foldl f (coll `f` head) tail
    Nothing          -> coll
  
  foldlStrict :: (a -> ElemType t -> a) -> a -> t -> a
  foldlStrict f = foldl (\a -> let fa = f a in seq (f a) (f a))
  
  foldl1 :: (ElemType t -> ElemType t -> ElemType t) -> t -> ElemType t
  foldl1 f cont = case view cont of
    Just (head,tail) -> foldl f head tail
    Nothing          -> error "Calling `foldl1` on empty collection."
  
-- foldl1Strict :: (ElemTyp t -> ElemTyp t -> ElemTyp t) -> t -> ElemTyp t
-- foldr :: (ElemTyp t -> b -> b) -> b -> t -> b
-- foldr1 :: (ElemTyp t -> ElemTyp t -> ElemTyp t) -> t -> ElemTyp t

  length :: Integral i => t -> i
  length = foldlStrict (const . (+1)) 0
 
  concat :: (Monoid (ElemType t)) => t -> ElemType t
  concat = foldl (+) empty

-- and :: Boolean (ElemTyp t) => t -> ElemTyp t
-- or :: Boolean (ElemTyp t) => t -> ElemTyp t
-- any :: (Function f, ArgTyp f ~ ElemTyp t, Boolean (ResTyp f)) => f -> t -> ElemTyp t
-- all :: (Function f, ArgTyp f ~ ElemTyp t, Boolean (ResTyp f)) => f -> t -> ElemTyp t

-- sum :: Num (ElemTyp t) => t -> (ElemTyp t)
-- product :: Num (ElemTyp t) => t -> (ElemTyp t)

-- maximum :: Ord (ElemTyp t) => t -> (ElemTyp t)
-- maximumBy :: (ElemTyp t -> ElemTyp t -> Ordering) -> t -> ElemTyp t
-- maximumOn :: Ord o => (ElemTyp t -> o) -> t -> ElemTyp t

-- minimum :: Ord (ElemTyp t) => t -> (ElemTyp t)
-- minimumBy :: (ElemTyp t -> ElemTyp t -> Ordering) -> t -> ElemTyp t
-- minimumOn :: Ord o => (ElemTyp t -> o) -> t -> ElemTyp t

-- mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
-- mapAccumR :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])

-- drop :: Int -> [a] -> [a]
-- dropWhile :: (a -> Bool) -> [a] -> [a]
-- dropWhileEnd :: (a -> Bool) -> [a] -> [a]

-- isPrefixOf :: Eq a => [a] -> [a] -> Bool
-- isSuffixOf :: Eq a => [a] -> [a] -> Bool
-- isInfixOf :: Eq a => [a] -> [a] -> Bool
-- elem :: Eq a => a -> [a] -> Bool
-- notElem :: Eq a => a -> [a] -> Bool
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- find :: (a -> Bool) -> [a] -> Maybe a

-- instance Associative

-- elemIndex :: Eq a => a -> [a] -> Maybe Int
-- findIndex :: (a -> Bool) -> [a] -> Maybe Int
