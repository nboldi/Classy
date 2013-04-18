
Name:
  - Classy is taken.
  - PMorph.Prelude

Ideas:
  - Generic plus?
  - String as class?

Questions:
  - Arrows or tuple operations?
  - Use own Numeric classes with {-# LANGUAGE RebindableSyntax #-}?
  
Design:
  - Debug module
  - Numeric modules
 
Document:
  - Eq, Ord: rules 
 
Constraint newtypes:
 - SortedTraversable t
 - Nonnegative a, Natural

Class instances:
  - Constructable + Traversable -> Monoid
  - Traversable t -> Indexed t Int (ElemTyp t)
  - Numeric -> Power
  - ComposableFunction -> Power
  
Category classes: 
  - class MultiplicativeMonoid g where unit, *
  - class Monoid g, MultiplicativeMonoid g => Ring g
  - class Ord n, Ring n => Numeric n
  - Group (a->a), Monad (a->b)
  
Container classes:
  - Traversable: +toList
  - class Constructable c where ConsElem c, cons
    - repeate, iterate, replicate
    - Traversable: take
    - fromList
  - class Traversable => Bidirectional where
    - last, init, viewBack
  - class Serializeable a s where serialize :: a -> s
    
Datatype classes:
  - Range (inRange, toRange, size), datatype (Traversable, Set, ...)
  - Heap (Traversable + insert, merge, decreaseKey), datatypes (from module)
  - Stack (push, pop, top), Queue (enqueue, dequeue, next)
  - class (Traversable a, Constructable a) => Set a (specializált lookup, remove, )
  - class Set a, Associative a => Multiset a where
  - Tree sets and maps will get Bidirectional instance
  
Control classes:
  - class Runnable r where run, eval, exec (instances for StateT, WriterT, RandomT)
  - class MonadFix
  
Generic classes:
  - class Power p where (^)
  - class Wrapped w where 
      type WrappedType w :: * 
      unwrap :: b -> (WrappedType w -> b) -> w -> b
  
Generalizing:
  - generalizing the given Monad instances for []
  - generalizing of id, const, flip


