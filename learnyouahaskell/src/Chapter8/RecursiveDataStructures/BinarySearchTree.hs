module Chapter8.RecursiveDataStructures.BinarySearchTree where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) 
  deriving (Show, Read, Eq) 
  
singelton :: a -> Tree a
singelton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singelton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x <  a = Node x (treeInsert x left) right
  | x >  a = Node x left (treeInsert x right)
  | otherwise = singelton x

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x emptyTree = False 
treeElem x (Node a left right) 
  | x == a = True 
  | x <  a = treeElem x left 
  | x >  a = treeElem x right