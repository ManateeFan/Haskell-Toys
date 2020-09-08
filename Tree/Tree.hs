module Tree ( Tree(..)) where
    
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton:: a -> Tree a
singleton a = Node a EmptyTree EmptyTree

treeInsert:: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node m left right) 
        | x == m = Node m left right
        | x < m  = Node m (treeInsert x left) right
        | otherwise  = Node m left (treeInsert x right)

treeElem:: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node m left right)
        | x == m = True
        | x > m = treeElem x right
        | x < m = treeElem x left

instance Functor Tree where
        fmap f EmptyTree = EmptyTree
        fmap f (Node v left right) = Node (f v) (fmap f left) (fmap f right)