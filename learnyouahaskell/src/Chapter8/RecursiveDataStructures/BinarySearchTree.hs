module Chapter8.RecursiveDataStructures.BinarySearchTree (Tree (..), singelton, treeInsert)where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) 
  deriving (Show, Read, Eq) 
  
singelton :: a -> Tree a
singelton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singelton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x <  a = Node a (treeInsert x left) right
  | x >  a = Node a left (treeInsert x right)
  | otherwise = EmptyTree

treeElem :: (Ord a) => a -> Tree a -> Bool 
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True 
  | x <  a = treeElem x left 
  | x >  a = treeElem x right
  | otherwise = False


---------------------------------------------------------

data Tree1 a = Empty
             | Leaf a
             | Branch a (Tree1 a) (Tree1 a)
             deriving (Eq, Show)

values :: Tree1 a -> [a]
values Empty = []
values (Leaf x) = [x]
values (Branch x l r) = values l ++ [x] ++ values r