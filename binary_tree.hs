data Tree a = Node a (Tree a) (Tree a)
            | Empty
 deriving (Show)

data Tree' a = Node' a (Maybe (Tree' a)) (Maybe (Tree' a))
 deriving (Show)


 --try to calculate the height of a tree
 --
height' :: Tree a -> Int
height' Empty = 0
height' (Node _ (x) (y)) = 1 + max (height' x) (height' y)

