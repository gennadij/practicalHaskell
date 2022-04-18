module Chapter8.Typeclasses102.FunctorTypeClass where

import Chapter8.RecursiveDataStructures.BinarySearchTree (Tree (..), singelton, treeInsert )

instance Functor Tree where 
  fmap f EmptyTree = EmptyTree 
  fmap f (Node x leftSub rightSub) = Node (f x) (fmap f leftSub) (fmap f rightSub)
