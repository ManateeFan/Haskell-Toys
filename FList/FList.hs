module FList
()
where

-- data FList a = Empty | Cons a (FList a) deriving (Show, Read, Eq, Ord)
infixr 5 :-: 
data FList a = Empty | a :-: (FList a) deriving (Show, Read, Eq, Ord)

infix 5 ^++

(^++) :: FList a -> FList a -> FList a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x:-:(xs ^++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton:: a -> Tree a
singleton a = Node a EmptyTree EmptyTree

treeInsert:: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node m left right) 
        | x == m = Node m left right
        | x < m = Node m (insertTree x left) right
        | x > m = Node m left (insertTree x right)

treeElem:: (ord a) => a -> Tree a -> Boll
treeElem x EmptyTree = False
treeElem x Node m left right 
        | x == m = True
        | x > m = treeElem x right
        | x < m = treeElem x left