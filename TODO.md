
Ideas:
  - Generic plus?
  - String as class?

Questions:
  - Arrows or tuple operations?
  - Runnable or Function instances?
  
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
  - class Constructable c where ConsElem c, cons
    - repeate, iterate, replicate
    - Traversable: take
    
Datatype classes:
  - range, datatype (Traversable, Set, ...)
  - heap (Traversable + insert, merge, decreaseKey), datatypes (from module)
  - Stack (push, pop, top), Queue (enqueue, dequeue, next)

Control classes:
  - class Runnable r where run, eval, exec (instances for StateT, WriterT, RandomT)
  
Generic classes:
  - class Power p where (^)
  
Generalizing:
  - generalizing the given Monad instances for []
  - generalizing of id, const, flip

