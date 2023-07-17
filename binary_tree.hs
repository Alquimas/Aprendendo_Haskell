data Tree a = Node a (Tree a) (Tree a)
            | Empty

instance (Show a) => Show (Tree a) where
    show Empty = " 'EMPTY' "
    show (Node b c d) = " '" ++ (show b) ++ "' (" ++ show c ++ show d ++ ")"

data Tree' a = Node' a (Maybe (Tree' a)) (Maybe (Tree' a))
 deriving (Show)


 --try to calculate the height of a tree
 --
height' :: (Show a) => Tree a -> Int
height' Empty = 0
height' (Node _ (x) (y)) = 1 + max (height' x) (height' y)

