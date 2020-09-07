module FList
(Flist(..))
where

-- data FList a = Empty | Cons a (FList a) deriving (Show, Read, Eq, Ord)
infixr 5 :-: 
data FList a = Empty | a :-: (FList a) deriving (Show, Read, Eq, Ord)

infix 5 ^++

(^++) :: FList a -> FList a -> FList a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x:-:(xs ^++ ys)

